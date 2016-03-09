{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
module Feldspar.Multicore.Compile.Parallella where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Monad.Operational.Higher
import Control.Monad.State
import Data.Bits
import qualified Data.Map as Map
import Data.Maybe
import Data.Proxy
import qualified Data.Set as Set
import Data.VirtualContainer
import GHC.TypeLits

import Feldspar.Multicore.Representation
import Feldspar.Representation
import Feldspar.Run
import Feldspar.Run.Compile

import qualified Language.C.Monad as C
import qualified Language.C.Quote as C
import qualified Language.C.Syntax as C
import Language.Embedded.Backend.C.Expression
import Language.Embedded.Expression
import qualified Language.Embedded.Imperative.CMD as Imp


onParallella :: (Run a -> b) -> AllocHost a -> b
onParallella action
    = action
    . wrapESDK
    . interpretWithMonad compAllocHostCMD
    . unAllocHost

--------------------------------------------------------------------------------
-- Transformation over Run
--------------------------------------------------------------------------------


-- TODO: add only the required number of cores to the group?
wrapESDK :: Allocator a -> Run a
wrapESDK program = do
    addInclude "<e-hal.h>"
    groupAddr <- addr . objArg <$> newNamedObject "group" "e_epiphany_t" False
    callProc "e_init" [ valArg (value 0 :: Data Int32) {- NULL -} ]
    callProc "e_reset_system" []
    callProc "e_open" [ groupAddr
                      , valArg (value 0 :: Data Int32)
                      , valArg (value 0 :: Data Int32)
                      , valArg (value 3 :: Data Int32)
                      , valArg (value 3 :: Data Int32) ]
    callProc "e_reset_group" [ groupAddr ]
    result <- evalStateT program (start groupAddr)
    callProc "e_close" [ groupAddr ]
    callProc "e_finalize" []
    return result


-- TODO: allocate only the arrays that are really used?
compAllocHostCMD :: CompExp exp => (AllocHostCMD exp) Allocator a -> Allocator a
compAllocHostCMD cmd@(Alloc size) = do
    let coreId = getAllocHostCoreId cmd
    let byteSize = size * 8 -- FIXME: calculate byte size from element type
    -- can we use sizeof() in C + addition of previous addresses? (shortly: no.)
    (addr, name) <- state (allocate coreId byteSize)
    (ty, incl) <- lift $ getResultType cmd
    modify (name `hasType` ty)
    modify (coreId `includes` incl)
    lift $ addDefinition [cedecl| typename off_t $id:name = $addr; |]
    lift $ mkArrayRef name
compAllocHostCMD (OnHost host) = do
    s <- get
    lift $ flip evalStateT s
         $ interpretWithMonadT compHostCMD lift
         $ unHost host

getAllocHostCoreId :: forall (coreId :: Nat) exp prog a . KnownNat coreId
                   => AllocHostCMD exp prog (LocalArr coreId a) -> CoreId
getAllocHostCoreId _ = fromIntegral $ natVal (Proxy :: Proxy coreId)


compHostCMD :: HostCMD Allocator a -> Allocator a
compHostCMD (Fetch dst (lower, upper) src) = do
    groupAddr <- gets group
    let srcName = arrayRefName src
        dstName = arrayRefName (unLocalArr dst)
    (r, c) <- gets $ groupCoordsForName dstName
    lift $ addInclude "<e-feldspar.h>"
    lift $ callProc "e_fetch"
        [ groupAddr
        , valArg $ value r
        , valArg $ value c
        , arrArg (unLocalArr dst)
        , arrArg src
        , valArg lower
        , valArg upper
        ]
compHostCMD (Flush src (lower, upper) dst) = do
    groupAddr <- gets group
    let srcName = arrayRefName (unLocalArr src)
        dstName = arrayRefName dst
    (r, c) <- gets $ groupCoordsForName srcName
    lift $ addInclude "<e-feldspar.h>"
    lift $ callProc "e_flush"
        [ groupAddr
        , valArg $ value r
        , valArg $ value c
        , arrArg (unLocalArr src)
        , arrArg dst
        , valArg lower
        , valArg upper
        ]
compHostCMD (OnCore comp) = do
    let coreId = getCoreCompCoreId comp
    let moduleName = "core" ++ show coreId
    s <- get
    lift $ inModule moduleName (compileCore coreId (unCoreComp comp) s)
    groupAddr <- gets group
    let (r, c) = groupCoord coreId
    lift $ addInclude "<e-loader.h>"
    lift $ callProc "e_load"
        [ strArg $ moduleName ++ ".srec"
        , groupAddr
        , valArg $ value r
        , valArg $ value c
        , valArg (value 1 :: Data Int32) --  E_TRUE
        ]

getCoreCompCoreId :: forall (coreId :: Nat) a . KnownNat coreId
           => CoreComp coreId a-> CoreId
getCoreCompCoreId cmd = fromIntegral $ natVal (Proxy :: Proxy coreId)


compileCore :: CoreId -> Comp () -> AllocatorState -> Run ()
compileCore coreId comp AllocatorState{..} = do
    -- compile the core program and collect the definition of main and used variables
    let (_, env) = C.runCGen
            (C.wrapMain $ interpret $ lowerTop $ liftRun $ comp)
            (C.defaultCEnv C.Flags)
        usedVars = maybe [] Set.toList (Map.lookup "main" $ C._funUsedVars env)

    -- collect pre-allocated scratchpad arrays
    let usedArrays = filter (isJust . snd)
               $ map (\(C.Id name _) -> (name, Map.lookup name nameMap)) usedVars
        arrayDefs = map (makeArrayDecl coreId typeMap) usedArrays

    -- merge type includes and array definitions
    let typeIncludes = maybe Set.empty id (Map.lookup coreId inclMap)
        env' = env { C._includes = C._includes env `Set.union` typeIncludes
                   , C._globals  = C._globals env ++ reverse arrayDefs }

    -- merge contents to the core module
    mapM_ addDefinition (C.cenvToCUnit env')

makeArrayDecl :: CoreId -> TypeMap -> (Name, Maybe (CoreId, LocalAddress)) -> Definition
makeArrayDecl coreId typeMap (name, Just (coreId', addr)) =
     let Just ty = Map.lookup name typeMap
     -- convert address to global when the given array is on another core
         addr' = if coreId' Prelude.== coreId then addr else addr `toGlobal` coreId'
     in  [cedecl| volatile $ty:ty * const $id:name = ($ty:ty *)$addr'; |]


--------------------------------------------------------------------------------
-- Utility functions to access lower layers
--------------------------------------------------------------------------------

getResultType :: (VarPred exp a, CompExp exp)
        => (AllocHostCMD exp) Allocator (proxy a)
        -> Run (C.Type, Set.Set String)
getResultType cmd = do
    let resultType = compTypeFromCMD cmd (proxyArg cmd)
        (ty, env) = C.runCGen resultType (C.defaultCEnv C.Flags)
    return (ty, C._includes env)

mkArrayRef :: SmallType a => VarId -> Run (LocalArr coreId a)
mkArrayRef name = return $ LocalArr $ Arr $ Actual $ Imp.ArrComp name

arrayRefName :: Arr a -> VarId
arrayRefName (Arr (Actual (Imp.ArrComp name))) = name


--------------------------------------------------------------------------------
-- Allocation state
--------------------------------------------------------------------------------

type Name = String
type LocalAddress = Word32
type AddressMap = Map.Map CoreId [(LocalAddress, Name)]
type NameMap = Map.Map Name (CoreId, LocalAddress)
type TypeMap = Map.Map Name C.Type
type IncludeMap = Map.Map CoreId (Set.Set String)
data AllocatorState = AllocatorState
    { group   :: FunArg Data
    , nextId  :: Int
    , addrMap :: AddressMap
    , nameMap :: NameMap
    , typeMap :: TypeMap
    , inclMap :: IncludeMap
    }
type Allocator = StateT AllocatorState Run


start :: FunArg Data -> AllocatorState
start g = AllocatorState
    { group = g
    , nextId = 0
    , addrMap = Map.empty
    , nameMap = Map.empty
    , typeMap = Map.empty
    , inclMap = Map.empty
    }

allocate :: CoreId -> Size -> AllocatorState -> ((LocalAddress, Name), AllocatorState)
allocate coreId size s@AllocatorState{..} = (newEntry, s
    { nextId = nextId + 1
    , addrMap = newAddrMap
    , nameMap = Map.insert newName (coreId, newAddress) nameMap
    })
  where
    newName = "spm" ++ show nextId
    (newAddress, _) = newEntry
    newEntry | Just (entry:_) <- Map.lookup coreId newAddrMap = entry
    newAddrMap = Map.alter (Just . stepAddress) coreId addrMap
    stepAddress (Just addrs@((lastAddress, _):_)) = (lastAddress + size, newName) : addrs
    stepAddress _ = [(bank2Base, newName)]

hasType :: Name -> C.Type -> AllocatorState -> AllocatorState
hasType name ty s@AllocatorState{..} = s { typeMap = Map.insert name ty typeMap }

includes :: CoreId -> Set.Set String -> AllocatorState -> AllocatorState
includes coreId incl s@AllocatorState{..} = s { inclMap = Map.alter merge coreId inclMap }
  where
    merge (Just i) = Just $ i `Set.union` incl
    merge _        = Just incl

groupCoordsForName :: Name -> AllocatorState -> CoreCoords
groupCoordsForName name AllocatorState{..}
    | Just (coreId, _) <- Map.lookup name nameMap =  groupCoord coreId


--------------------------------------------------------------------------------
-- Hardware specific utilities
--------------------------------------------------------------------------------

type CoreCoords = (Word32, Word32)

groupCoord :: CoreId -> CoreCoords
groupCoord coreId
    | isValidCoreId coreId = coreId `divMod` 4
    | otherwise = error $ "invalid core id: " ++ show coreId

isValidCoreId :: CoreId -> Bool
isValidCoreId = flip elem [0..15]

systemCoord :: CoreId -> CoreCoords
systemCoord coreId = let (gr, gc) = groupCoord coreId in (gr + 32, gc + 8)


type GlobalAddress = Word32

toGlobal :: LocalAddress -> CoreId -> GlobalAddress
toGlobal addr coreId =
    let (sr, sc) = systemCoord coreId
    -- 6 bit row number, 6 bit column number, 20 bit local address
    in (sr `shift` 26) .|. (sc `shift` 20) .|. addr

bank2Base :: LocalAddress
bank2Base = 0x2000
