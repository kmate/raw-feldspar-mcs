module Feldspar.Multicore.Frontend where

import Control.Monad.Operational.Higher
import GHC.TypeLits

import Feldspar
import Feldspar.Multicore.Representation


--------------------------------------------------------------------------------
-- Core layer
--------------------------------------------------------------------------------

getLArr :: (KnownNat coreId, Syntax a)
        => Data Index -> LocalArr coreId (Internal a) -> CoreComp coreId a
getLArr i = getArr i . unLocalArr

setLArr :: (KnownNat coreId, Syntax a, MonadComp m)
        => Data Index -> a -> LocalArr coreId (Internal a) -> m ()
setLArr i a = setArr i a . unLocalArr


--------------------------------------------------------------------------------
-- Host layer
--------------------------------------------------------------------------------

fetch :: (KnownNat coreId, SmallType a)
      => LocalArr coreId a -> IndexRange -> Arr a -> Host ()
fetch = fetchTo 0

flush :: (KnownNat coreId, SmallType a)
      => LocalArr coreId a -> IndexRange -> Arr a -> Host ()
flush = flushFrom 0

fetchTo  :: (KnownNat coreId, SmallType a)
         => Data Index -> LocalArr coreId a -> IndexRange -> Arr a -> Host ()
fetchTo offset spm range = Host . singleInj . Fetch spm offset range

flushFrom :: (KnownNat coreId, SmallType a)
          => Data Index -> LocalArr coreId a -> IndexRange -> Arr a -> Host ()
flushFrom offset spm range = Host . singleInj . Flush spm offset range

onCore :: KnownNat coreId => CoreComp coreId () -> Host ()
onCore = Host . singleInj . OnCore


--------------------------------------------------------------------------------
-- Allocation layer
--------------------------------------------------------------------------------

alloc :: (KnownNat coreId, SmallType a)
      => Size -> Multicore (LocalArr coreId a)
alloc = Multicore . singleE . Alloc

onHost :: Host a -> Multicore a
onHost = Multicore . singleE . OnHost
