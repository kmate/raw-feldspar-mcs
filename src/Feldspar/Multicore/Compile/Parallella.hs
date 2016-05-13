{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
module Feldspar.Multicore.Compile.Parallella where

import Control.Monad.Operational.Higher
import Control.Monad.Reader
import Control.Monad.State
import Data.Bits
import qualified Data.Map as Map
import Data.Maybe
import Data.Proxy
import qualified Data.Set as Set
import Data.TypedStruct

import Feldspar.Multicore.Representation
import Feldspar.Primitive.Representation
import Feldspar.Representation
import Feldspar.Run hiding ((==), (.|.), mod)
import Feldspar.Run.Concurrent
import Feldspar.Run.Compile
import Feldspar.Run.Representation

import qualified Language.C.Monad as C
import qualified Language.C.Quote as C
import qualified Language.C.Quote.C as C
import qualified Language.C.Syntax as C
import Language.Embedded.Backend.C.Expression
import Language.Embedded.Expression
import qualified Language.Embedded.Imperative.CMD as Imp


icompileAll :: MonadRun m => m a -> IO ()
icompileAll  = mapM_ (\(n, m) -> putStrLn ("// module " ++ n) >> putStrLn m) . compileAll

onParallella :: (Run a -> b) -> Multicore a -> b
onParallella action
    = action
    . wrapESDK
    . interpret
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
    callProc "e_reset_group" [ groupAddr ]
    callProc "e_close" [ groupAddr ]
    callProc "e_finalize" []
    return result


--------------------------------------------------------------------------------
-- Allocation layer
--------------------------------------------------------------------------------

-- TODO: allocate only the arrays that are really used?
compAllocCMD :: (CompExp Prim, CompTypeClass PrimType)
             => AllocCMD (Param3 RunGen Prim PrimType) a -> RunGen a
compAllocCMD cmd@(AllocSArr size) = do
    (arr, size) <- compAlloc cmd sharedId size
    shmRef <- addr . objArg <$> (lift $ newNamedObject "shm" "e_mem_t" False)
    modify (arrayRefName arr `describes` shmRef)
    lift $ callProc "e_alloc" [ shmRef, arrArg $ unwrapArr arr, valArg $ value size ]
    return arr
compAllocCMD cmd@(AllocLArr coreId size) = fst <$> compAlloc cmd coreId size
compAllocCMD (OnHost host) = do
    s <- get
    lift $ evalGen s $ interpretT (lift :: Run a -> RunGen a) $ unHost host

compAlloc :: (ArrayWrapper arr, CompExp Prim, CompTypeClass PrimType, PrimType a)
          => AllocCMD (Param3 RunGen Prim PrimType) (arr a)
          -> CoreId
          -> Size
          -> RunGen (arr a, Length)
compAlloc cmd coreId size = do
    let (ty, incl) = getArrElemType cmd
        byteSize   = size * sizeOf ty
    (addr, name) <- state (allocate coreId byteSize)
    modify (name `hasType` ty)
    modify (coreId `includes` incl)
    lift $ addDefinition [cedecl| typename off_t $id:name = $addr; |]
    return (mkArrayRef name, byteSize)

getArrElemType :: (CompExp Prim, CompTypeClass PrimType, PrimType a)
           => AllocCMD (Param3 RunGen Prim PrimType) (proxy a)
           -> (C.Type, Set.Set String)
getArrElemType cmd =
    let (ty, env) = cGen $ compType (proxyPred cmd) (proxyArg cmd)
    in  (ty, C._includes env)

instance Interp AllocCMD RunGen (Param2 Prim PrimType)
  where interp = compAllocCMD


--------------------------------------------------------------------------------
-- Host layer
--------------------------------------------------------------------------------

class Monad m => ControlGen m
  where
    evalGen  :: RGState -> m a -> Run a
    genState :: m RGState
    fromRun  :: Run a -> m a

compControlCMD :: ControlGen m
               => Imp.ControlCMD (Param3 m Data PrimType') a
               -> m a
compControlCMD (Imp.If cond t f)     = do
    s <- genState
    let t' = evalGen s t
        f' = evalGen s f
    fromRun $ iff cond t' f'
compControlCMD (Imp.For range body)  = do
    s <- genState
    let body' = evalGen s . body
    fromRun $ Run $ singleInj $ Imp.For range (unRun . body')
compControlCMD (Imp.While cond body) = do
    s <- genState
    let cond' = evalGen s cond
        body' = evalGen s body
    fromRun $ while cond' body'

instance Interp Imp.ControlCMD RunGen (Param2 Data PrimType')
  where interp = compControlCMD


compHostWaitCMD :: WaitCMD (Param3 RunGen exp pred) a -> RunGen a
compHostWaitCMD BusyWait = lift $ delayThread (100 :: Data Int32)

instance Interp WaitCMD RunGen (Param2 exp pred)
  where interp = compHostWaitCMD


compLocalBulkArrCMD :: (BulkArrCMD LocalArr) (Param3 RunGen exp pred) a -> RunGen a
compLocalBulkArrCMD (WriteArr offset spm range ram) =
    compLocalCopy "host_write_local"  spm ram offset range
compLocalBulkArrCMD (ReadArr  offset spm range ram) =
    compLocalCopy "host_read_local"   spm ram offset range

instance Interp (BulkArrCMD LocalArr) RunGen (Param2 exp pred)
  where interp = compLocalBulkArrCMD

compLocalCopy :: PrimType a => String
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
        , arrArg (unwrapArr spm)
        , arrArg ram
        , valArg offset
        , valArg lower
        , valArg upper
        ]


compSharedBulkArrCMD :: (BulkArrCMD SharedArr) (Param3 RunGen exp pred) a -> RunGen a
compSharedBulkArrCMD (WriteArr offset spm range ram) =
    compSharedCopy "host_write_shared" spm ram offset range
compSharedBulkArrCMD (ReadArr  offset spm range ram) =
    compSharedCopy "host_read_shared"  spm ram offset range

instance Interp (BulkArrCMD SharedArr) RunGen (Param2 exp pred)
  where interp = compSharedBulkArrCMD

compSharedCopy :: PrimType a => String
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


compMulticoreCMD :: MulticoreCMD (Param3 RunGen exp pred) a -> RunGen a
compMulticoreCMD (OnCore coreId comp) = do
    s <- genState
    compCore coreId
        $ evalGen s
        $ interpretT ((lift :: Run a -> CoreGen a) . liftRun)
        $ unCoreComp comp
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

instance Interp MulticoreCMD RunGen (Param2 exp pred)
  where interp = compMulticoreCMD

moduleName :: CoreId -> String
moduleName = ("core" ++) . show

compCore :: CoreId -> Run () -> RunGen ()
compCore coreId comp = do
    -- compile the core program to C and collect the resulting environment
    let (_, env) = cGen $ C.wrapMain $ interpret $ translate comp

    -- collect pre-allocated local and shared arrays used by core main
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
    return $ if coreId' == sharedId
       then [cedecl| volatile void * const $id:name = (void *)$addr; |]
       else [cedecl| volatile $ty:ty * const $id:name = ($ty:ty *)$addr'; |]


--------------------------------------------------------------------------------
-- Core layer
--------------------------------------------------------------------------------

instance Interp Imp.ControlCMD CoreGen (Param2 Data PrimType')
  where interp = compControlCMD


compCoreWaitCMD :: Monad m => WaitCMD (Param3 m exp pred) a -> m a
compCoreWaitCMD BusyWait = return ()

instance Interp WaitCMD CoreGen (Param2 exp pred)
  where interp = compCoreWaitCMD


compCoreLocalBulkArrCMD :: (BulkArrCMD LocalArr) (Param3 CoreGen exp pred) a -> CoreGen a
compCoreLocalBulkArrCMD (WriteArr offset spm range ram) =
    compCoreLocalCopy "core_write_local"  spm ram offset range
compCoreLocalBulkArrCMD (ReadArr  offset spm range ram) =
    compCoreLocalCopy "core_read_local"   spm ram offset range

instance Interp (BulkArrCMD LocalArr) CoreGen (Param2 exp pred)
  where interp = compCoreLocalBulkArrCMD

compCoreLocalCopy :: PrimType a => String
                  -> LocalArr a-> Arr a
                  -> Data Index -> IndexRange -> CoreGen ()
compCoreLocalCopy op spm ram offset (lower, upper) = do
    groupAddr <- asks group
    (r, c) <- asks $ groupCoordsForName (arrayRefName spm)
    lift $ do
        addInclude "<string.h>"
        addInclude "<e-lib.h>"
        addInclude "<e-feldspar.h>"
        callProc op
            [ arrArg (unwrapArr spm)
            , arrArg ram
            , valArg offset
            , valArg lower
            , valArg upper
            ]


compCoreSharedBulkArrCMD :: (BulkArrCMD SharedArr) (Param3 CoreGen exp pred) a -> CoreGen a
compCoreSharedBulkArrCMD (WriteArr offset spm range ram) =
    compCoreSharedCopy "core_write_shared" spm ram offset range
compCoreSharedBulkArrCMD (ReadArr  offset spm range ram) =
    compCoreSharedCopy "core_read_shared"  spm ram offset range

instance Interp (BulkArrCMD SharedArr) CoreGen (Param2 exp pred)
  where interp = compCoreSharedBulkArrCMD

compCoreSharedCopy :: PrimType a => String
                   -> SharedArr a-> Arr a
                   -> Data Index -> IndexRange -> CoreGen ()
compCoreSharedCopy op spm ram offset (lower, upper) = do
    shmRef <- asks $ shmRefForName $ arrayRefName spm
    lift $ do
        addInclude "<e-lib.h>"
        addInclude "<e-feldspar.h>"
        callProc op
            [ arrArg (unwrapArr spm)
            , arrArg ram
            , valArg offset
            , valArg lower
            , valArg upper
            ]


--------------------------------------------------------------------------------
-- Utility functions to access lower layers
--------------------------------------------------------------------------------

cGen :: C.CGen a -> (a, C.CEnv)
cGen = flip C.runCGen (C.defaultCEnv C.Flags)

mkArrayRef :: (ArrayWrapper arr, PrimType a) => VarId -> arr a
mkArrayRef = wrapArr . Arr . Single . Imp.ArrComp

arrayRefName :: ArrayWrapper arr => arr a -> VarId
arrayRefName (unwrapArr -> (Arr (Single (Imp.ArrComp name)))) = name


--------------------------------------------------------------------------------
-- Host compiler state and utilities
--------------------------------------------------------------------------------

type Name = String
type LocalAddress = Word32
type AddressMap = Map.Map CoreId [(LocalAddress, LocalAddress, Name)]
type NameMap = Map.Map Name (CoreId, LocalAddress)
type TypeMap = Map.Map Name C.Type
type IncludeMap = Map.Map CoreId (Set.Set String)
type SharedMemRef = FunArg Data PrimType'
type SharedMemMap = Map.Map Name SharedMemRef
data RGState = RGState
    { group   :: FunArg Data PrimType'
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

instance ControlGen RunGen
  where
    evalGen  = flip evalStateT
    genState = get
    fromRun  = lift

start :: FunArg Data PrimType' -> RGState
start g = RGState
    { group   = g
    , nextId  = 0
    , addrMap = Map.empty
    , nameMap = Map.empty
    , typeMap = Map.empty
    , inclMap = Map.empty
    , shmMap  = Map.empty
    }

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
-- Core compiler representation and utilities
--------------------------------------------------------------------------------

type CoreGen = ReaderT RGState Run

instance ControlGen CoreGen
  where
    evalGen  = flip runReaderT
    genState = ask
    fromRun  = lift


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
toGlobal addr coreId
    -- external memory addresses are relative to a fixed base pointer
    | coreId == sharedId = addr
    | otherwise = let (sr, sc) = systemCoord coreId
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
sizeOf (isCTypeOf (Proxy :: Proxy Bool)             -> True) = 1
sizeOf (isCTypeOf (Proxy :: Proxy Int8)             -> True) = 1
sizeOf (isCTypeOf (Proxy :: Proxy Int16)            -> True) = 2
sizeOf (isCTypeOf (Proxy :: Proxy Int32)            -> True) = 4
sizeOf (isCTypeOf (Proxy :: Proxy Int64)            -> True) = 8
sizeOf (isCTypeOf (Proxy :: Proxy Word8)            -> True) = 1
sizeOf (isCTypeOf (Proxy :: Proxy Word16)           -> True) = 2
sizeOf (isCTypeOf (Proxy :: Proxy Word32)           -> True) = 4
sizeOf (isCTypeOf (Proxy :: Proxy Word64)           -> True) = 8
sizeOf (isCTypeOf (Proxy :: Proxy Float)            -> True) = 4
sizeOf (isCTypeOf (Proxy :: Proxy Double)           -> True) = 8
sizeOf (isCTypeOf (Proxy :: Proxy (Complex Float))  -> True) = 8
sizeOf (isCTypeOf (Proxy :: Proxy (Complex Double)) -> True) = 16
sizeOf cty = error $ "size of C type is unknown: " ++ show cty

isCTypeOf :: CType a => proxy a -> C.Type -> Bool
isCTypeOf ty cty = cty == cTypeOf ty

cTypeOf :: CType a => proxy a -> C.Type
cTypeOf = fst . cGen . cType

instance CType (Complex Float)
  where
    cType _ = C.addSystemInclude "tgmath.h" >> return [C.cty| _Complex float |]
    cLit (r :+ 0) = return [C.cexp| $r |]
    cLit (0 :+ i) = return [C.cexp| $i * I |]
    cLit (r :+ i) = return [C.cexp| $r + $i * I |]

instance CType (Complex Double)
  where
    cType _ = C.addSystemInclude "tgmath.h" >> return [C.cty| _Complex double |]
    cLit (r :+ 0) = return [C.cexp| $r |]
    cLit (0 :+ i) = return [C.cexp| $i * I |]
    cLit (r :+ i) = return [C.cexp| $r + $i * I |]
