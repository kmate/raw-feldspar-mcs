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
import Feldspar.Run hiding ((==))
import Feldspar.Run.Compile

import qualified Language.C.Monad as C
import qualified Language.C.Quote as C
import qualified Language.C.Syntax as C
import qualified Language.Embedded.CExp as Exp
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
wrapESDK :: RunGen a -> Run a
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
    result <- runGen (start groupAddr) program
    callProc "e_close" [ groupAddr ]
    callProc "e_finalize" []
    return result


-- TODO: allocate only the arrays that are really used?
compAllocHostCMD :: CompExp exp => (AllocHostCMD exp) RunGen a -> RunGen a
compAllocHostCMD cmd@(Alloc size) = do
    let (ty, incl) = getResultType cmd
        byteSize   = size * sizeOf ty
        coreId     = getAllocHostCoreId cmd
    (addr, name) <- state (allocate coreId byteSize)
    modify (name `hasType` ty)
    modify (coreId `includes` incl)
    lift $ addDefinition [cedecl| typename off_t $id:name = $addr; |]
    return $ mkArrayRef name
compAllocHostCMD (OnHost host) = do
    s <- get
    lift $ runGen s $ interpretT lift $ unHost host

getAllocHostCoreId :: forall (coreId :: Nat) exp prog a . KnownNat coreId
                   => AllocHostCMD exp prog (LocalArr coreId a) -> CoreId
getAllocHostCoreId _ = fromIntegral $ natVal (Proxy :: Proxy coreId)


compControlCMD :: (Imp.ControlCMD Data) RunGen a -> RunGen a
compControlCMD (Imp.If cond t f)     = do
    s <- get
    let t' = runGen s t
        f' = runGen s f
    lift $ iff cond t' f'
compControlCMD (Imp.For range body)  = do
    s <- get
    let body' = runGen s . body
    lift$ for range body'
compControlCMD (Imp.While cond body) = do
    s <- get
    let cond' = runGen s cond
        body' = runGen s body
    lift $ while cond' body'

instance Interp (Imp.ControlCMD Data) RunGen where interp = compControlCMD

compMulticoreCMD :: MulticoreCMD RunGen a -> RunGen a
compMulticoreCMD (Fetch spm range ram) = compCopy "e_fetch" spm ram range
compMulticoreCMD (Flush spm range ram) = compCopy "e_flush" spm ram range
compMulticoreCMD (OnCore comp) = do
    let coreId = getCoreCompCoreId comp
    compCore coreId (unCoreComp comp)
    groupAddr <- gets group
    let (r, c) = groupCoord coreId
    lift $ addInclude "<e-loader.h>"
    lift $ callProc "e_load"
        [ strArg $ moduleName coreId ++ ".srec"
        , groupAddr
        , valArg $ value r
        , valArg $ value c
        , valArg (value 1 :: Data Int32) {- E_TRUE -}
        ]

instance Interp MulticoreCMD RunGen where interp = compMulticoreCMD


compCopy :: SmallType a => String -> LocalArr coreId a-> Arr a -> IndexRange -> RunGen ()
compCopy op spm ram (lower, upper) = do
    groupAddr <- gets group
    (r, c) <- gets $ groupCoordsForName (arrayRefName (unLocalArr spm))
    lift $ addInclude "<e-feldspar.h>"
    lift $ callProc op
        [ groupAddr
        , valArg $ value r
        , valArg $ value c
        , arrArg (unLocalArr spm)
        , arrArg ram
        , valArg lower
        , valArg upper
        ]

getCoreCompCoreId :: forall (coreId :: Nat) a . KnownNat coreId
                  => CoreComp coreId a-> CoreId
getCoreCompCoreId cmd = fromIntegral $ natVal (Proxy :: Proxy coreId)

moduleName :: CoreId -> String
moduleName = ("core" ++) . show

compCore :: CoreId -> Comp () -> RunGen ()
compCore coreId comp = do
    -- compile the core program to C and collect the resulting environment
    let (_, env) = cGen $ C.wrapMain $ interpret $ lowerTop $ liftRun comp

    -- collect pre-allocated scratchpad arrays used by core main
    arrayDecls <- mkArrayDecls coreId (mainUsedVars env)

    -- merge type includes and array definitions
    inclMap <- gets inclMap
    let typeIncludes = fromMaybe Set.empty (Map.lookup coreId inclMap)
        env' = env { C._includes = C._includes env `Set.union` typeIncludes
                   -- cenvToCUnit will reverse the order of definitions
                   , C._globals  = C._globals env ++ reverse arrayDecls }

    -- merge contents to the core module
    lift $ inModule (moduleName coreId)
         $ mapM_ addDefinition (C.cenvToCUnit env')

mainUsedVars :: C.CEnv -> [Name]
mainUsedVars
    = map (\(C.Id name _) -> name)
    . maybe [] Set.toList
    . Map.lookup "main"
    . C._funUsedVars

mkArrayDecls :: CoreId -> [Name] -> RunGen [Definition]
mkArrayDecls coreId usedVars = do
    nameMap <- gets nameMap
    let arrayVars = filter (isJust . flip Map.lookup nameMap) usedVars
    forM arrayVars $ mkArrayDecl coreId

mkArrayDecl :: CoreId -> Name -> RunGen Definition
mkArrayDecl coreId name = do
    typeMap <- gets typeMap
    nameMap <- gets nameMap
    let Just ty = Map.lookup name typeMap
        Just (coreId', addr) = Map.lookup name nameMap
     -- convert address to global when the given array is on another core
        addr'
            | coreId' == coreId = addr
            | otherwise = addr `toGlobal` coreId'
    return $ [cedecl| volatile $ty:ty * const $id:name = ($ty:ty *)$addr'; |]


--------------------------------------------------------------------------------
-- Utility functions to access lower layers
--------------------------------------------------------------------------------

cGen :: C.CGen a -> (a, C.CEnv)
cGen = flip C.runCGen (C.defaultCEnv C.Flags)

getResultType :: (VarPred exp a, CompExp exp)
              => (AllocHostCMD exp) RunGen (proxy a)
              -> (C.Type, Set.Set String)
getResultType cmd =
    let (ty, env) = cGen $ compTypeFromCMD cmd (proxyArg cmd)
    in  (ty, C._includes env)

mkArrayRef :: SmallType a => VarId -> LocalArr coreId a
mkArrayRef name = LocalArr $ Arr $ Actual $ Imp.ArrComp name

arrayRefName :: Arr a -> VarId
arrayRefName (Arr (Actual (Imp.ArrComp name))) = name


--------------------------------------------------------------------------------
-- Compiler state and utilities
--------------------------------------------------------------------------------

type Name = String
type LocalAddress = Word32
type AddressMap = Map.Map CoreId [(LocalAddress, LocalAddress, Name)]
type NameMap = Map.Map Name (CoreId, LocalAddress)
type TypeMap = Map.Map Name C.Type
type IncludeMap = Map.Map CoreId (Set.Set String)
data RGState = RGState
    { group   :: FunArg Data
    , nextId  :: Int
    , addrMap :: AddressMap
    , nameMap :: NameMap
    , typeMap :: TypeMap
    , inclMap :: IncludeMap
    }
type RunGen = StateT RGState Run


runGen :: RGState -> RunGen a -> Run a
runGen = flip evalStateT

start :: FunArg Data -> RGState
start g = RGState
    { group   = g
    , nextId  = 0
    , addrMap = Map.empty
    , nameMap = Map.empty
    , typeMap = Map.empty
    , inclMap = Map.empty
    }

allocate :: CoreId -> Size -> RGState -> ((LocalAddress, Name), RGState)
allocate coreId size s@RGState{..} = ((startAddr, newName), s
    { nextId = nextId + 1
    , addrMap = newAddrMap
    , nameMap = Map.insert newName (coreId, startAddr) nameMap
    })
  where
    newName = "spm" ++ show nextId
    (startAddr, _, _) = newEntry
    newEntry | Just (entry:_) <- Map.lookup coreId newAddrMap = entry
    newAddrMap = Map.alter (Just . addAddr) coreId addrMap
    addAddr (Just addrs@((_, next, _):_)) = (next, next + size', newName) : addrs
    addAddr _ = [(bank2Base, bank2Base + size', newName)]
    size' = wordAlign size

hasType :: Name -> C.Type -> RGState -> RGState
hasType name ty s = s { typeMap = Map.insert name ty (typeMap s) }

includes :: CoreId -> Set.Set String -> RGState -> RGState
includes coreId incl s = s { inclMap = Map.alter merge coreId (inclMap s) }
  where
    merge (Just i) = Just $ i `Set.union` incl
    merge _        = Just incl

groupCoordsForName :: Name -> RGState -> CoreCoords
groupCoordsForName name RGState{..}
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

wordAlign :: LocalAddress -> LocalAddress
wordAlign addr
    | m == 0 = addr
    | otherwise = addr - m + 4
    where m = addr `mod` 4

bank2Base :: LocalAddress
bank2Base = 0x2000


sizeOf :: C.Type -> Size
sizeOf (isCTypeOf (Proxy :: Proxy Bool)   -> True) = 1
sizeOf (isCTypeOf (Proxy :: Proxy Int8)   -> True) = 1
sizeOf (isCTypeOf (Proxy :: Proxy Int16)  -> True) = 2
sizeOf (isCTypeOf (Proxy :: Proxy Int32)  -> True) = 4
sizeOf (isCTypeOf (Proxy :: Proxy Int64)  -> True) = 8
sizeOf (isCTypeOf (Proxy :: Proxy Word8)  -> True) = 1
sizeOf (isCTypeOf (Proxy :: Proxy Word16) -> True) = 2
sizeOf (isCTypeOf (Proxy :: Proxy Word32) -> True) = 4
sizeOf (isCTypeOf (Proxy :: Proxy Word64) -> True) = 8
sizeOf (isCTypeOf (Proxy :: Proxy Float)  -> True) = 4
sizeOf (isCTypeOf (Proxy :: Proxy Double) -> True) = 8
sizeOf cty = error $ "size of C type is unknown: " ++ show cty

isCTypeOf :: Exp.CType a => proxy a -> C.Type -> Bool
isCTypeOf ty cty = cty == cTypeOf ty

cTypeOf :: Exp.CType a => proxy a -> C.Type
cTypeOf = fst . cGen . Exp.cType
