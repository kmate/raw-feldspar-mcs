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
import qualified Data.Set as Set
import Data.VirtualContainer

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


-- IDEA: write an abstraction layer above ESDK to make the code generation easier?
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
    result <- (flip evalStateT) (start groupAddr) program
    callProc "e_close" [ groupAddr ]
    callProc "e_finalize" []
    return result


compAllocHostCMD :: CompExp exp => (AllocHostCMD exp) Allocator a -> Allocator a
compAllocHostCMD cmd@(Alloc coreId size) = do
    ty <- lift $ getResultType cmd
    let byteSize = size -- FIXME: calculate byte size from element type
    -- can we use sizeof() in C + addition of previous addresses?
    (addr, name) <- state (allocate coreId byteSize)
    modify (name `hasType` ty)
    lift $ addDefinition [cedecl| volatile $ty:ty * const $id:name = $addr; |]
    lift $ mkArrayRef name
compAllocHostCMD (OnHost host) = do
    s <- get
    lift $ (flip evalStateT) s
         $ interpretWithMonadT compHostCMD lift
         $ unHost $ host


compHostCMD :: HostCMD Allocator a -> Allocator a
compHostCMD (Fetch dst (lower, upper) src) = do
    groupAddr <- gets group
    let srcName = arrayRefName src
        dstName = arrayRefName dst
    (r, c) <- gets $ groupCoordsForName dstName
    lift $ callProc "e_write"
        [ groupAddr
        , valArg $ value r
        , valArg $ value c
        , arrArg dst
        , arrArg src
        -- e_write(&group, 0, 0, d0, input, N * sizeof(uint32_t));
        {- TODO: add size arg and offset to source -} ]
compHostCMD (Flush src (lower, upper) dst) = do
    groupAddr <- gets group
    let srcName = arrayRefName src
        dstName = arrayRefName dst
    (r, c) <- gets $ groupCoordsForName srcName
    lift $ callProc "e_read"
        [ groupAddr
        , valArg $ value r
        , valArg $ value c
        , arrArg src
        , arrArg dst
        -- e_read(&group, 0, 2, d2, output, N * sizeof(uint32_t));
        {- TODO: add size arg and offset to destination -} ]
compHostCMD (OnCore coreId comp) = do
    (coreName, coreDefs) <- compileCore coreId comp
    -- FIXME: these definitions should go into another translation unit
    lift $ mapM addDefinition coreDefs
    groupAddr <- gets group
    let (r, c) = groupCoord coreId
    lift $ addInclude "<e-loader.h>"
    lift $ callProc "e_load"
        [ strArg $ coreName ++ ".srec"
        , groupAddr
        , valArg $ value r
        , valArg $ value c
        , valArg (value 1 :: Data Int32) --  E_TRUE
        ]

compileCore :: CoreId -> Comp () -> Allocator (Name, [Definition])
compileCore coreId comp = do
    arrays <- Map.lookup coreId  <$> gets addrMap
    globals <- case arrays of
            Just addrs -> mapM (makeGlobalArr coreId) addrs
            _          -> return []
    let (coreMainDef, _) = C.runCGen
            (wrapCore $ interpret $ lowerTop $ liftRun $ comp)
            (C.defaultCEnv C.Flags)
    return ("core" ++ show coreId, globals ++ [coreMainDef])

makeGlobalArr :: CoreId -> (LocalAddress, Name) -> Allocator Definition
makeGlobalArr coreId (addr, name) = do
    Just ty <- Map.lookup name <$> gets typeMap
    let gaddr = addr `toGlobal` coreId
    return [cedecl| volatile $ty:ty * const $id:name = $gaddr; |]

wrapCore :: C.MonadC m => m () -> m Definition
wrapCore prog = do
    (_,uvs,params,items) <- C.inNewFunction $ prog -- >> C.addStm [cstm| return 0; |]
    C.setUsedVars "main" uvs
    return [cedecl| int main($params:params){ $items:items }|]


--------------------------------------------------------------------------------
-- Utility functions to access lower layers
--------------------------------------------------------------------------------

getResultType :: (VarPred exp a, CompExp exp)
        => (AllocHostCMD exp) Allocator (proxy a)
        -> Run C.Type
getResultType cmd = do
    let resultType = compTypeFromCMD cmd (proxyArg cmd)
        (ty, env) = C.runCGen resultType (C.defaultCEnv C.Flags)
    mapM addInclude (Set.toList (C._includes env))
    return ty

mkArrayRef :: SmallType a => VarId -> Run (Arr a)
mkArrayRef name = return $ Arr $ Actual $ Imp.ArrComp name

arrayRefName :: Arr a -> VarId
arrayRefName (Arr (Actual (Imp.ArrComp name))) = name


--------------------------------------------------------------------------------
-- Allocation state
--------------------------------------------------------------------------------

type Name = String
type LocalAddress = Word32
type AddressMap = Map.Map CoreId [(LocalAddress, Name)]
type NameMap = Map.Map Name CoreId
type TypeMap = Map.Map Name C.Type
data AllocatorState = AllocatorState
    { group   :: FunArg Data
    , nextId  :: Int
    , addrMap :: AddressMap
    , nameMap :: NameMap
    , typeMap :: TypeMap
    }
type Allocator = StateT AllocatorState Run


start :: FunArg Data -> AllocatorState
start g = AllocatorState
    { group = g
    , nextId = 0
    , addrMap = Map.empty
    , nameMap = Map.empty
    , typeMap = Map.empty
    }

allocate :: CoreId -> Size -> AllocatorState -> ((LocalAddress, Name), AllocatorState)
allocate coreId size s@AllocatorState{..} = (newEntry, s
    { nextId = nextId + 1
    , addrMap = newAddrMap
    , nameMap = Map.insert newName coreId nameMap
    })
  where
    newName = "spm" ++ show nextId
    newEntry | Just (entry:_) <- Map.lookup coreId newAddrMap = entry
    newAddrMap = Map.alter (Just . stepAddress) coreId addrMap
    stepAddress (Just addrs@((lastAddress, _):_)) = (lastAddress + size, newName) : addrs
    stepAddress _ = [(bank2Base, newName)]

hasType :: Name -> C.Type -> AllocatorState -> AllocatorState
hasType name ty s@AllocatorState{..} = s { typeMap = Map.insert name ty typeMap }

groupCoordsForName :: Name -> AllocatorState -> CoreCoords
groupCoordsForName name AllocatorState{..}
    | Just coreId <- Map.lookup name nameMap =  groupCoord coreId


--------------------------------------------------------------------------------
-- Hardware specific utilities
--------------------------------------------------------------------------------

type CoreCoords = (Word32, Word32)

groupCoord :: CoreId -> CoreCoords
groupCoord coreId
    | isValidCoreId coreId = coreId `divMod` 4
    | otherwise = error $ "invalid core id: " ++ show coreId

isValidCoreId :: CoreId -> Bool
isValidCoreId = (flip elem) [0..15]

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
