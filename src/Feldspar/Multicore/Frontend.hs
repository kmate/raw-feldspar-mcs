module Feldspar.Multicore.Frontend
    ( alloc, runHost
    , fetch, flush, onCore
    )where

import Control.Monad.Operational.Higher
import Control.Monad.Trans

import Data.VirtualContainer

import Feldspar
import Feldspar.Run
import Feldspar.Multicore.Representation


--------------------------------------------------------------------------------
-- Host layer
--------------------------------------------------------------------------------

fetch :: Type a => Arr a -> IndexRange -> Arr a -> HostT m ()
fetch dst range = Host . singleInj . Fetch dst range

flush :: Type a => Arr a -> IndexRange -> Arr a -> HostT m ()
flush src range = Host . singleInj . Flush src range

onCore :: CoreId -> Comp () -> HostT Run ()
onCore coreId = Host . singleInj . OnCore coreId


--------------------------------------------------------------------------------
-- Allocation layer
--------------------------------------------------------------------------------

alloc :: Type a => CoreId -> Size -> AllocHost (Arr a)
alloc coreId = singleInj . Alloc coreId

runHost :: HostT Run a -> AllocHost a
runHost = singleInj . RunHost
