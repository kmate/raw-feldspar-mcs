module Feldspar.Multicore.Frontend where

import Control.Monad.Operational.Higher

import Feldspar
import Feldspar.Multicore.CoreId
import Feldspar.Multicore.Representation

import Language.Embedded.Concurrent as Imp


forever :: MonadComp m => m () -> m ()
forever = while (return true)


--------------------------------------------------------------------------------
-- Bulk array frontend
--------------------------------------------------------------------------------

writeArr :: (ArrayAccess arr m, PrimType a)
         => arr (Data a) -> IndexRange -> DArr a -> m ()
writeArr = writeArrAt 0

readArr :: (ArrayAccess arr m, PrimType a)
        => arr (Data a) -> IndexRange -> DArr a -> m ()
readArr = readArrAt 0

class ArrayWrapper arr => ArrayAccess arr m
  where
    writeArrAt :: PrimType a => Data Index -> arr (Data a) -> IndexRange -> DArr a -> m ()
    readArrAt  :: PrimType a => Data Index -> arr (Data a) -> IndexRange -> DArr a -> m ()


--------------------------------------------------------------------------------
-- Halting core
--------------------------------------------------------------------------------

class Halt m
  where
    haltCore :: CoreRef -> m ()


--------------------------------------------------------------------------------
-- Core layer
--------------------------------------------------------------------------------

instance ArrayAccess LArr CoreComp
  where
    writeArrAt offset spm range = CoreComp . singleInj . WriteArr offset spm range
    readArrAt  offset spm range = CoreComp . singleInj . ReadArr  offset spm range

instance ArrayAccess SArr CoreComp
  where
    writeArrAt offset spm range = CoreComp . singleInj . WriteArr offset spm range
    readArrAt  offset spm range = CoreComp . singleInj . ReadArr  offset spm range


local :: LArr a -> CoreComp (Arr a)
local = return . unLArr

getLArr :: Syntax a => LArr a -> Data Index -> CoreComp a
getLArr arr i = do
  larr <- local arr
  getArr larr i

setLArr :: forall a. Syntax a => LArr a -> Data Index -> a -> CoreComp ()
setLArr arr i a = do
  larr <- local arr
  setArr larr i a

instance Halt CoreComp
  where
    haltCore = CoreComp . singleInj . HaltCore


--------------------------------------------------------------------------------
-- Host layer
--------------------------------------------------------------------------------

onCoreWithRef :: CoreId -> (CoreRef -> CoreComp ()) -> Host CoreRef
onCoreWithRef coreId = Host . singleInj . OnCore coreId

onCore :: CoreId -> CoreComp () -> Host CoreRef
onCore coreId = onCoreWithRef coreId . const


instance ArrayAccess LArr Host
  where
    writeArrAt offset spm range = Host . singleInj . WriteArr offset spm range
    readArrAt  offset spm range = Host . singleInj . ReadArr  offset spm range

instance ArrayAccess SArr Host
  where
    writeArrAt offset spm range = Host . singleInj . WriteArr offset spm range
    readArrAt  offset spm range = Host . singleInj . ReadArr  offset spm range


instance Halt Host
  where
    haltCore = Host . singleInj . HaltCore


forkWithId :: (ThreadId -> Host ()) -> Host ThreadId
forkWithId = Host . Imp.forkWithId . (unHost .)


--------------------------------------------------------------------------------
-- Allocation layer
--------------------------------------------------------------------------------

allocLArr :: PrimType a => CoreId -> Length -> Multicore (DLArr a)
allocLArr coreId = Multicore . singleInj . AllocLArr coreId

allocSArr :: PrimType a => Length -> Multicore (DSArr a)
allocSArr = Multicore . singleInj . AllocSArr

onHost :: Host a -> Multicore a
onHost = Multicore . singleInj . OnHost
