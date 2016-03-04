module Feldspar.Multicore.Frontend
    ( alloc, runHost
    , fetch, flush, onCore, host
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
fetch dst r src = undefined

flush :: Type a => Arr a -> Range (Data Index) -> Arr a -> Host ()
flush src r dst = undefined

onCore :: MonadComp m => CoreId -> m () -> Host ()
onCore c = undefined

host :: Run a -> Host a
host = Host


--------------------------------------------------------------------------------
-- Allocation layer
--------------------------------------------------------------------------------

alloc :: Type a => CoreId -> Size -> AllocHost (Arr a)
alloc c = singleInj . Alloc c

runHost :: Host a -> AllocHost a
runHost = singleInj . RunHost
