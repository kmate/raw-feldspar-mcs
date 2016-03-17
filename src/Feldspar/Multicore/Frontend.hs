module Feldspar.Multicore.Frontend where

import Control.Monad.Operational.Higher

import Feldspar
import Feldspar.Multicore.Representation


--------------------------------------------------------------------------------
-- Core layer
--------------------------------------------------------------------------------

local :: LocalArr a -> CoreComp (Arr a)
local = return . unLocalArr

(-<) :: (Arr a -> CoreComp b) -> LocalArr a -> CoreComp b
action -< arr = action =<< local arr

infixr 1 -<

forever :: CoreComp () -> CoreComp ()
forever = while (return $ true)


--------------------------------------------------------------------------------
-- Host layer
--------------------------------------------------------------------------------

writeArr :: SmallType a => LocalArr a -> IndexRange -> Arr a -> Host ()
writeArr = writeArrAt 0

readArr :: SmallType a => LocalArr a -> IndexRange -> Arr a -> Host ()
readArr = readArrAt 0

writeArrAt :: SmallType a
           => Data Index -> LocalArr a
           -> IndexRange -> Arr a -> Host ()
writeArrAt offset spm range = Host . singleInj . WriteArr offset spm range

readArrAt :: SmallType a
          => Data Index -> LocalArr a
          -> IndexRange -> Arr a -> Host ()
readArrAt offset spm range = Host . singleInj . ReadArr offset spm range

onCore :: CoreId -> CoreComp () -> Host ()
onCore coreId = Host . singleInj . OnCore coreId


--------------------------------------------------------------------------------
-- Allocation layer
--------------------------------------------------------------------------------

allocArr :: SmallType a => CoreId -> Size -> Multicore (LocalArr a)
allocArr coreId = Multicore . singleE . AllocArr coreId

onHost :: Host a -> Multicore a
onHost = Multicore . singleE . OnHost
