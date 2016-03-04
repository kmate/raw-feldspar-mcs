module Feldspar.Multicore.Frontend
    ( alloc, runHost
    , fetch, flush, onCore, run
    )where

import Control.Monad.Operational.Higher

import Data.VirtualContainer

import Feldspar
import Feldspar.Run
import Feldspar.Multicore.Representation


--------------------------------------------------------------------------------
-- Host layer
--------------------------------------------------------------------------------

-- TODO: implement

fetch :: Type a => Arr a -> Range (Data Index) -> Arr a -> Host ()
fetch larr r = undefined

flush :: Type a => Arr a -> Range (Data Index) -> Arr a -> Host ()
flush larr r = undefined

onCore :: MonadComp m => CoreId -> m () -> Host ()
onCore c = undefined

run :: Run a -> Host a
run = Host

--------------------------------------------------------------------------------
-- Allocation layer
--------------------------------------------------------------------------------

alloc :: SmallType a => CoreId -> Size -> AllocHost (Arr a)
alloc c = singleInj . Alloc c

runHost :: Host a -> AllocHost a
runHost = singleInj . RunHost
