module Feldspar.Multicore.Frontend where

import Control.Monad.Operational.Higher
import Control.Monad.Trans

import Data.VirtualContainer
import GHC.TypeLits

import Feldspar
import Feldspar.Run
import Feldspar.Multicore.Representation


--------------------------------------------------------------------------------
-- Core layer
--------------------------------------------------------------------------------

getLArr :: forall (coreId :: Nat) a m . (KnownNat coreId, Syntax a)
        => Data Index -> LocalArr coreId (Internal a) -> CoreComp coreId a
getLArr i = getArr i . unLocalArr

setLArr :: forall (coreId :: Nat) a m . (KnownNat coreId, Syntax a, MonadComp m)
        => Data Index -> a -> LocalArr coreId (Internal a) -> m ()
setLArr i a = setArr i a . unLocalArr


--------------------------------------------------------------------------------
-- Host layer
--------------------------------------------------------------------------------

fetch :: (KnownNat coreId, SmallType a)
      => LocalArr coreId a -> IndexRange -> Arr a -> Host ()
fetch dst range = Host . singleInj . Fetch dst range

flush :: forall (coreId :: Nat) a . (KnownNat coreId, SmallType a)
      =>LocalArr coreId a -> IndexRange -> Arr a -> Host ()
flush src range = Host . singleInj . Flush src range

onCore :: forall (coreId :: Nat) . KnownNat coreId => CoreComp coreId () -> Host ()
onCore = Host . singleInj . OnCore


--------------------------------------------------------------------------------
-- Allocation layer
--------------------------------------------------------------------------------

alloc :: forall (coreId :: Nat) a . (KnownNat coreId, SmallType a)
      => Size -> AllocHost (LocalArr coreId a)
alloc = AllocHost . singleE . Alloc

onHost :: Host a -> AllocHost a
onHost = AllocHost . singleE . OnHost
