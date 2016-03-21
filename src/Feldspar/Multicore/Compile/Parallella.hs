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


onParallella :: (Run a -> b) -> Multicore a -> b
onParallella action
    = action
    . wrapESDK
    . interpretWithMonad compAllocCMD
    . unMulticore

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
                      , valArg (value 4 :: Data Int32)
                      , valArg (value 4 :: Data Int32) ]
    callProc "e_reset_group" [ groupAddr ]
    (result, state) <- runGen (start groupAddr) program
    let sharedTypeIncludes = fromMaybe Set.empty (Map.lookup sharedId (inclMap state))
    mapM_ addInclude sharedTypeIncludes
    mapM_ (callProc "e_free" . return . snd) (Map.toList $ shmMap state)
    callProc "e_close" [ groupAddr ]
    callProc "e_finalize" []
    return result


-- TODO: allocate only the arrays that are really used?
compAllocCMD :: CompExp exp => (AllocCMD exp) RunGen a -> RunGen a
compAllocCMD cmd@(AllocSArr        size) = do
    (arr, size) <- compAlloc cmd sharedId size
    shmRef <- addr . objArg <$> (lift $ newNamedObject "shm" "e_mem_t" False)
    modify (arrayRefName arr `describes` shmRef)
    lift $ callProc "e_alloc" [ shmRef, arrArg $ unwrap arr, valArg $ value size ]
    return arr
compAllocCMD cmd@(AllocLArr coreId size) = fst <$> compAlloc cmd coreId size
compAllocCMD (OnHost host) = do
    s <- get
    lift $ evalGen s $ interpretT lift $ unHost host

compAlloc :: (ArrayWrapper arr, CompExp exp, VarPred exp a, SmallType a)
          => (AllocCMD exp) RunGen (arr a) -> CoreId -> Size -> RunGen (arr a, Length)
compAlloc cmd coreId size = do
    let (ty, incl) = getResultType cmd
        byteSize   = size * sizeOf ty
    (addr, name) <- state (allocate coreId byteSize)
    modify (name `hasType` ty)
    modify (coreId `includes` incl)
    lift $ addDefinition [cedecl| typename off_t $id:name = $addr; |]
    return (mkArrayRef name, byteSize)


compControlCMD :: (Imp.ControlCMD Data) RunGen a -> RunGen a
compControlCMD (Imp.If cond t f)     = do
    s <- get
    let t' = evalGen s t
        f' = evalGen s f
    lift $ iff cond t' f'
compControlCMD (Imp.For range body)  = do
    s <- get
    let body' = evalGen s . body
    lift$ for range body'
compControlCMD (Imp.While cond body) = do
    s <- get
    let cond' = evalGen s cond
        body' = evalGen s body
    lift $ while cond' body'

instance Interp (Imp.ControlCMD Data) RunGen where interp = compControlCMD


compLocalBulkArrCMD :: (BulkArrCMD LocalArr) RunGen a -> RunGen a
compLocalBulkArrCMD (WriteArr offset spm range ram) =
    compLocalCopy "e_write_local"  spm ram offset range
compLocalBulkArrCMD (ReadArr  offset spm range ram) =
    compLocalCopy "e_read_local"   spm ram offset range

instance Interp (BulkArrCMD LocalArr) RunGen where interp = compLocalBulkArrCMD

compLocalCopy :: SmallType a => String
              -> LocalArr a-> Arr a
              -> Data Index -> IndexRange -> RunGen ()
compLocalCopy op spm ram offset (lower, upper) = do
    groupAddr <- gets group
    (r, c) <- gets $ groupCoordsForName (arrayRefName spm)
    lift $ addInclude "<e-feldspar.h>"
    lift $ callProc op
        [ groupAddr
        , valArg $ value r
        , valArg $ value c
        , arrArg (unwrap spm)
        , arrArg ram
        , valArg offset
        , valArg lower
        , valArg upper
        ]


compSharedBulkArrCMD :: (BulkArrCMD SharedArr) RunGen a -> RunGen a
compSharedBulkArrCMD (WriteArr offset spm range ram) =
    compSharedCopy "e_write_shared" spm ram offset range
compSharedBulkArrCMD (ReadArr  offset spm range ram) =
    compSharedCopy "e_read_shared"  spm ram offset range

instance Interp (BulkArrCMD SharedArr) RunGen where interp = compSharedBulkArrCMD

compSharedCopy :: SmallType a => String
               -> SharedArr a-> Arr a
               -> Data Index -> IndexRange -> RunGen ()
compSharedCopy op spm ram offset (lower, upper) = do
    shmRef <- gets $ shmRefForName $ arrayRefName spm
    lift $ addInclude "<e-feldspar.h>"
    lift $ callProc op
        [ shmRef
        , arrArg ram
        , valArg offset
        , valArg lower
        , valArg upper
        ]


compMulticoreCMD :: MulticoreCMD RunGen a -> RunGen a
compMulticoreCMD (OnCore coreId comp) = do
    compCore coreId $ runCoreComp comp
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
    let coreTypeIncludes   = fromMaybe Set.empty (Map.lookup coreId   inclMap)
        sharedTypeIncludes = fromMaybe Set.empty (Map.lookup sharedId inclMap)
        env' = env { C._includes = C._includes env
                         `Set.union` coreTypeIncludes
                         `Set.union` sharedTypeIncludes
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
              => (AllocCMD exp) RunGen (proxy a)
              -> (C.Type, Set.Set String)
getResultType cmd =
    let (ty, env) = cGen $ compTypeFromCMD cmd (proxyArg cmd)
    in  (ty, C._includes env)

mkArrayRef :: (ArrayWrapper arr, SmallType a) => VarId -> arr a
mkArrayRef = wrap . Arr . Actual . Imp.ArrComp

arrayRefName :: ArrayWrapper arr => arr a -> VarId
arrayRefName (unwrap -> (Arr (Actual (Imp.ArrComp name)))) = name


--------------------------------------------------------------------------------
-- Compiler state and utilities
--------------------------------------------------------------------------------

type Name = String
type LocalAddress = Word32
type AddressMap = Map.Map CoreId [(LocalAddress, LocalAddress, Name)]
type NameMap = Map.Map Name (CoreId, LocalAddress)
type TypeMap = Map.Map Name C.Type
type IncludeMap = Map.Map CoreId (Set.Set String)
type SharedMemRef = FunArg Data
type SharedMemMap = Map.Map Name SharedMemRef
data RGState = RGState
    { group   :: FunArg Data
    , nextId  :: Int
    , addrMap :: AddressMap
    , nameMap :: NameMap
    , typeMap :: TypeMap
    , inclMap :: IncludeMap
    , shmMap  :: SharedMemMap
    }
type RunGen = StateT RGState Run


runGen :: RGState -> RunGen a -> Run (a, RGState)
runGen = flip runStateT

evalGen :: RGState -> RunGen a -> Run a
evalGen = flip evalStateT

start :: FunArg Data -> RGState
start g = RGState
    { group   = g
    , nextId  = 0
    , addrMap = Map.empty
    , nameMap = Map.empty
    , typeMap = Map.empty
    , inclMap = Map.empty
    , shmMap  = Map.empty
    }

sharedId :: CoreId
sharedId = maxBound  -- use the maximum representable value for shared addresses

allocate :: CoreId -> Size -> RGState -> ((LocalAddress, Name), RGState)
allocate coreId size s@RGState{..} = ((startAddr, newName), s
    { nextId  = nextId + 1
    , addrMap = newAddrMap
    , nameMap = Map.insert newName (coreId, startAddr) nameMap
    })
  where
    isShared = sharedId == coreId
    newName  = (if isShared then "sa" else "la") ++ show nextId
    (startAddr, _, _) = newEntry
    newEntry | Just (entry:_) <- Map.lookup coreId newAddrMap = entry
    newAddrMap = Map.alter (Just . addAddr) coreId addrMap
    addAddr (Just addrs@((_, next, _):_)) = (next, next + size', newName) : addrs
    addAddr _ = [(baseAddr, baseAddr + size', newName)]
    baseAddr = if isShared then sharedBase else bank1Base
    size' = wordAlign size

describes :: Name -> SharedMemRef -> RGState -> RGState
describes name shmAddr s = s { shmMap = Map.insert name shmAddr (shmMap s) }

hasType :: Name -> C.Type -> RGState -> RGState
hasType name ty s = s { typeMap = Map.insert name ty (typeMap s) }

includes :: CoreId -> Set.Set String -> RGState -> RGState
includes coreId incl s = s { inclMap = Map.alter merge coreId (inclMap s) }
  where
    merge (Just i) = Just $ i `Set.union` incl
    merge _        = Just incl

groupCoordsForName :: Name -> RGState -> CoreCoords
groupCoordsForName name RGState{..}
    | Just (coreId, _) <- Map.lookup name nameMap = groupCoord coreId

shmRefForName :: Name -> RGState -> SharedMemRef
shmRefForName name RGState{..}
    | Just shmRef <- Map.lookup name shmMap = shmRef

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

bank1Base :: LocalAddress  -- local in the address space of a core
bank1Base = 0x2000

sharedBase :: LocalAddress  -- relative to external memory base
sharedBase = 0x1000000


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
