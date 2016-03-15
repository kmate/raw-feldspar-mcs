module Feldspar.Multicore.Frontend where

import Control.Monad.Operational.Higher

import Feldspar
import Feldspar.Multicore.Representation


--------------------------------------------------------------------------------
-- Host layer
--------------------------------------------------------------------------------

fetch :: SmallType a => Arr a -> IndexRange -> Arr a -> Host ()
fetch spm range = Host . singleInj . Fetch spm range

flush :: SmallType a => Arr a -> IndexRange -> Arr a -> Host ()
flush spm range = Host . singleInj . Flush spm range

onCore :: CoreId -> Comp () -> Host ()
onCore coreId = Host . singleInj . OnCore coreId


--------------------------------------------------------------------------------
-- Allocation layer
--------------------------------------------------------------------------------

alloc :: SmallType a => CoreId -> Size -> Multicore (Arr a)
alloc coreId = Multicore . singleE . Alloc coreId

onHost :: Host a -> Multicore a
onHost = Multicore . singleE . OnHost
