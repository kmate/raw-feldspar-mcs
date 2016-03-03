module Feldspar.Multicore.Frontend
    ( LArr
    , alloc, runHost
    , fetch, flush, onCore
    , getLArr, setLArr
    )where

import Control.Monad.Operational.Higher

import Data.Ix
import Data.VirtualContainer

import Feldspar
import Feldspar.Multicore.Representation
import Feldspar.Representation

import Language.Embedded.Expression


--------------------------------------------------------------------------------
-- Core layer
--------------------------------------------------------------------------------

newtype LArr a = LArr { unLArr :: Virtual SmallType (LocalArr Index) a }


getLArr'
    :: ( VarPred (IExp instr) a
       , VarPred (IExp instr) i
       , Integral i
       , Ix i
       , LocalArrCMD (IExp instr) :<: instr
       )
    => IExp instr i -> LocalArr i a -> ProgramT instr m (IExp instr a)
getLArr' i = singleE . GetLArr i

getLArr :: Syntax a => Data Index -> LArr (Internal a) -> CoreComp a
getLArr i = fmap resugar . mapVirtualA (CoreComp . getLArr' i) . unLArr


setLArr'
    :: ( VarPred (IExp instr) a
       , VarPred (IExp instr) i
       , Integral i
       , Ix i
       , LocalArrCMD (IExp instr) :<: instr
       )
    => IExp instr i -> IExp instr a -> LocalArr i a -> ProgramT instr m ()
setLArr' i a = singleE . SetLArr i a

setLArr :: forall a . Syntax a => Data Index -> a -> LArr (Internal a) -> CoreComp ()
setLArr i a
    = sequence_
    . zipListVirtual (\a' arr' -> CoreComp $ setLArr' i a' arr') rep
    . unLArr
  where
    rep = resugar a :: Virtual SmallType Data (Internal a)


--------------------------------------------------------------------------------
-- Host layer
--------------------------------------------------------------------------------

-- TODO: implement fetch and flush

fetch'
    :: ( VarPred (IExp instr) a
       , VarPred (IExp instr) i
       , Integral i
       , Ix i
       , MulticoreCMD (IExp instr) :<: instr
       )
    => LocalArr i a -> Range i -> Arr a -> ProgramT instr m ()
fetch' larr r = singleE . Fetch larr r

fetch :: LArr a -> Range (Data Index) -> Arr a -> Host ()
fetch larr r = undefined


flush'
    :: ( VarPred (IExp instr) a
       , VarPred (IExp instr) i
       , Integral i
       , Ix i
       , MulticoreCMD (IExp instr) :<: instr
       )
    => LocalArr i a -> Range i -> Arr a -> ProgramT instr m ()
flush' larr r = singleE . Flush larr r

flush :: LArr a -> Range (Data Index) -> Arr a -> Host ()
flush larr r = undefined


onCore :: MonadComp m => CoreId -> m () -> Host ()
onCore c = Host . singleE . OnCore c


--------------------------------------------------------------------------------
-- Allocation layer
--------------------------------------------------------------------------------

alloc'
    :: ( VarPred (IExp instr) a
       , VarPred (IExp instr) i
       , Integral i
       , Ix i
       , AllocCMD (IExp instr) :<: instr
       )
    => CoreId -> Size -> ProgramT instr m (LocalArr i a)
alloc' c = singleE . Alloc c

alloc :: SmallType a => CoreId -> Size -> AllocHost (LArr a)
alloc c = fmap (LArr . Actual) . alloc' c


runHost :: Host a -> AllocHost a
runHost = singleInj . RunHost
