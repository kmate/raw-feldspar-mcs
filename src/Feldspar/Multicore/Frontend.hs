module Feldspar.Multicore.Frontend where

import Control.Monad.Operational.Higher

import Feldspar
import Feldspar.Multicore.Representation

import GHC.TypeLits


-- TODO: do we need to add constraints on `a` below?

alloc :: forall (coreId :: Nat) a. Size -> AllocHost (LocalArr coreId a)
alloc size = singleInj $ Alloc size

runHost :: Host a -> AllocHost a
runHost = singleInj . RunHost

fetch :: forall (coreId :: Nat) a. LocalArr coreId a -> Range -> Arr a -> Host ()
fetch localArr range = Host . singleInj . Fetch localArr range

flush :: forall (coreId :: Nat) a. LocalArr coreId a -> Range -> Arr a -> Host ()
flush localArr range = Host . singleInj . Flush localArr range

onCore :: MonadComp m => m () -> Host ()
onCore = Host . singleInj . OnCore


-- TODO: implement these functions

getLArr :: forall (coreId :: Nat) a m. Syntax a =>
    Data Index -> LocalArr coreId (Internal a) -> CoreComp coreId a
getLArr i = undefined

unsafeGetLArr :: forall (coreId :: Nat) a m. Syntax a =>
    Data Index -> LocalArr coreId (Internal a) -> CoreComp coreId a
unsafeGetLArr i = undefined

setLArr :: forall (coreId :: Nat) a m. (Syntax a, MonadComp m) =>
    Data Index -> a -> LocalArr coreId (Internal a) -> m ()
setLArr = undefined
