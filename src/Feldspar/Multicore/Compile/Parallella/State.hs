{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
module Feldspar.Multicore.Compile.Parallella.State where

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set

import Feldspar.Multicore.Compile.Parallella.Access
import Feldspar.Multicore.Compile.Parallella.Esdk
import Feldspar.Multicore.Compile.Parallella.Imports

import qualified Language.C.Monad as C (_includes, CGen)
import Language.C.Quote.C
import qualified Language.C.Syntax as C (Type)
import Language.Embedded.Imperative.Frontend.General (FunArg)
import qualified Language.Embedded.Imperative.CMD as Imp (ControlCMD (..))


--------------------------------------------------------------------------------
-- Host compiler state and utilities
--------------------------------------------------------------------------------

type Name = String
type AddressMap = Map.Map CoreId [(LocalAddress, LocalAddress, Name)]
type NameMap = Map.Map Name (CoreId, LocalAddress)
type TypeMap = Map.Map Name C.Type
type IncludeMap = Map.Map CoreId (Set.Set String)
type SharedMemRef = FunArg Data PrimType'
type SharedMemMap = Map.Map Name SharedMemRef
type CoreInit = Map.Map CoreId (RunGen ())
type Started = Set.Set CoreId

data RGState = RGState
    { group    :: FunArg Data PrimType'
    , nextId   :: Int
    , addrMap  :: AddressMap
    , nameMap  :: NameMap
    , typeMap  :: TypeMap
    , inclMap  :: IncludeMap
    , shmMap   :: SharedMemMap
    , coreInit :: CoreInit
    , started  :: Started
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
    { group    = g
    , nextId   = 0
    , addrMap  = Map.empty
    , nameMap  = Map.empty
    , typeMap  = Map.empty
    , inclMap  = Map.empty
    , shmMap   = Map.empty
    , coreInit = Map.empty
    , started  = Set.empty
    }

allocate :: CoreId -> Length -> RGState -> ((LocalAddress, Name), RGState)
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
    size' = aligned size

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

addInit :: CoreId -> RunGen () -> RunGen ()
addInit coreId init = do
    execute <- Set.member coreId <$> gets started
    if execute
        then init
        else modify $ \s -> s { coreInit = update $ coreInit s }
  where
    update = Map.insertWith combine coreId init
    combine new existing = sequence_ [ existing, new ]

markStarted :: CoreId -> RunGen ()
markStarted coreId = modify $ \s -> s { started = Set.insert coreId $ started s }


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
-- Common support for control structure compilation
--------------------------------------------------------------------------------

class Monad m => ControlGen m
  where
    evalGen  :: RGState -> m a -> Run a
    genState :: m RGState
    fromRun  :: Run a -> m a

compControlCMD :: ControlGen m
               => Imp.ControlCMD (Param3 m Data PrimType') a
               -> m a
compControlCMD (Imp.If cond t f) = do
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

instance Interp Imp.ControlCMD CoreGen (Param2 Data PrimType')
  where interp = compControlCMD


--------------------------------------------------------------------------------
-- General shared and local array allocation
--------------------------------------------------------------------------------

allocShared :: (ArrayWrapper arr, PrimType a)
             => C.CGen C.Type -> Length -> RunGen (arr (Data a), FunArg Data PrimType')
allocShared cty size = do
    (arr, size) <- allocLocal cty sharedId size
    shmRef <- addr . objArg <$> (lift $ newNamedObject "shm" "e_mem_t" False)
    modify (arrayRefName arr `describes` shmRef)
    lift $ callProc "e_alloc" [ shmRef, arrArg $ unwrapArr arr, valArg $ value size ]
    return (arr, shmRef)

allocLocal :: (ArrayWrapper arr, PrimType a)
          => C.CGen C.Type -> CoreId -> Length -> RunGen (arr (Data a), Length)
allocLocal cty coreId size = do
    let (ty, env) = cGen cty
        incl = C._includes env
        byteSize   = size * sizeOf ty
    (addr, name) <- state (allocate coreId byteSize)
    modify (name `hasType` ty)
    modify (coreId `includes` incl)
    lift $ addDefinition [cedecl| typename off_t $id:name = $addr; |]
    return (mkArrayRef name size, byteSize)
