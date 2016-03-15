module Feldspar.Multicore.Frontend where

import Control.Monad.Operational.Higher

import Feldspar
import Feldspar.Multicore.Representation


--------------------------------------------------------------------------------
-- Host layer
--------------------------------------------------------------------------------

fetch :: SmallType a => Arr a -> IndexRange -> Arr a -> Host ()
fetch = fetchTo 0

flush :: SmallType a => Arr a -> IndexRange -> Arr a -> Host ()
flush = flushFrom 0

fetchTo :: SmallType a => Data Index -> Arr a -> IndexRange -> Arr a -> Host ()
fetchTo offset spm range = Host . singleInj . Fetch spm offset range

flushFrom :: SmallType a => Data Index -> Arr a -> IndexRange -> Arr a -> Host ()
flushFrom offset spm range = Host . singleInj . Flush spm offset range

onCore :: CoreId -> Comp () -> Host ()
onCore coreId = Host . singleInj . OnCore coreId


--------------------------------------------------------------------------------
-- Allocation layer
--------------------------------------------------------------------------------

alloc :: SmallType a => CoreId -> Size -> Multicore (Arr a)
alloc coreId = Multicore . singleE . Alloc coreId

onHost :: Host a -> Multicore a
onHost = Multicore . singleE . OnHost
