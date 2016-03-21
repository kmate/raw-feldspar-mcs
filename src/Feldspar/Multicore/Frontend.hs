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

writeArr :: (ArrayAccess arr m, SmallType a) => arr a -> IndexRange -> Arr a -> m ()
writeArr = writeArrAt 0

readArr :: (ArrayAccess arr m, SmallType a) => arr a -> IndexRange -> Arr a -> m ()
readArr = readArrAt 0

class ArrayAccess arr m
  where
    writeArrAt :: SmallType a => Data Index -> arr a -> IndexRange -> Arr a -> m ()
    readArrAt  :: SmallType a => Data Index -> arr a -> IndexRange -> Arr a -> m ()

instance ArrayAccess LocalArr Host
  where
    writeArrAt offset spm range = Host . singleInj . WriteLArr offset spm range
    readArrAt offset spm range = Host . singleInj . ReadLArr offset spm range

instance ArrayAccess SharedArr Host
  where
    writeArrAt offset spm range = Host . singleInj . WriteSArr offset spm range
    readArrAt offset spm range = Host . singleInj . ReadSArr offset spm range


onCore :: CoreId -> CoreComp () -> Host ()
onCore coreId = Host . singleInj . OnCore coreId


--------------------------------------------------------------------------------
-- Allocation layer
--------------------------------------------------------------------------------

allocLArr :: SmallType a => CoreId -> Size -> Multicore (LocalArr a)
allocLArr coreId = Multicore . singleE . AllocLArr coreId

allocSArr :: SmallType a => Size -> Multicore (SharedArr a)
allocSArr = Multicore . singleE . AllocSArr

onHost :: Host a -> Multicore a
onHost = Multicore . singleE . OnHost
