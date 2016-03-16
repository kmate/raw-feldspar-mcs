module Feldspar.Multicore.Frontend where

import Control.Monad.Operational.Higher

import Feldspar
import Feldspar.Multicore.Representation


--------------------------------------------------------------------------------
-- Host layer
--------------------------------------------------------------------------------

writeArr :: SmallType a => Arr a -> IndexRange -> Arr a -> Host ()
writeArr = writeArrAt 0

readArr :: SmallType a => Arr a -> IndexRange -> Arr a -> Host ()
readArr = readArrAt 0

writeArrAt :: SmallType a => Data Index -> Arr a -> IndexRange -> Arr a -> Host ()
writeArrAt offset spm range = Host . singleInj . WriteArr offset spm range

readArrAt :: SmallType a => Data Index -> Arr a -> IndexRange -> Arr a -> Host ()
readArrAt offset spm range = Host . singleInj . ReadArr offset spm range

onCore :: CoreId -> CoreComp () -> Host ()
onCore coreId = Host . singleInj . OnCore coreId


--------------------------------------------------------------------------------
-- Allocation layer
--------------------------------------------------------------------------------

allocArr :: SmallType a => CoreId -> Size -> Multicore (Arr a)
allocArr coreId = Multicore . singleE . AllocArr coreId

onHost :: Host a -> Multicore a
onHost = Multicore . singleE . OnHost
