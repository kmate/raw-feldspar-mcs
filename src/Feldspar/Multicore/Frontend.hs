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
fetch spm range = Host . singleInj . Fetch spm range

flush :: (KnownNat coreId, SmallType a)
      => LocalArr coreId a -> IndexRange -> Arr a -> Host ()
flush spm range = Host . singleInj . Flush spm range

onCore :: KnownNat coreId => CoreComp coreId () -> Host ()
onCore = Host . singleInj . OnCore


--------------------------------------------------------------------------------
-- Allocation layer
--------------------------------------------------------------------------------

alloc :: (KnownNat coreId, SmallType a)
      => Size -> AllocHost (LocalArr coreId a)
alloc = AllocHost . singleE . Alloc

onHost :: Host a -> AllocHost a
onHost = AllocHost . singleE . OnHost
