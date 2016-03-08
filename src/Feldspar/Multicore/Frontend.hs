module Feldspar.Multicore.Frontend
    ( alloc, onHost
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

fetch :: SmallType a => Arr a -> IndexRange -> Arr a -> Host ()
fetch dst range = Host . singleInj . Fetch dst range

flush :: SmallType a => Arr a -> IndexRange -> Arr a -> Host ()
flush src range = Host . singleInj . Flush src range

onCore :: CoreId -> Comp () -> Host ()
onCore coreId = Host . singleInj . OnCore coreId


--------------------------------------------------------------------------------
-- Allocation layer
--------------------------------------------------------------------------------

alloc :: SmallType a => CoreId -> Size -> AllocHost (Arr a)
alloc coreId = AllocHost . singleE . Alloc coreId

onHost :: Host a -> AllocHost a
onHost = AllocHost . singleE . OnHost
