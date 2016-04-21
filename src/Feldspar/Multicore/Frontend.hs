module Feldspar.Multicore.Frontend where

import Control.Monad.Operational.Higher

import Feldspar
import Feldspar.Multicore.Representation


--------------------------------------------------------------------------------
-- Bulk array frontend
--------------------------------------------------------------------------------

writeArr :: (ArrayAccess arr m, PrimType a) => arr a -> IndexRange -> Arr a -> m ()
writeArr = writeArrAt 0

readArr :: (ArrayAccess arr m, PrimType a) => arr a -> IndexRange -> Arr a -> m ()
readArr = readArrAt 0

class ArrayWrapper arr => ArrayAccess arr m
  where
    writeArrAt :: PrimType a => Data Index -> arr a -> IndexRange -> Arr a -> m ()
    readArrAt  :: PrimType a => Data Index -> arr a -> IndexRange -> Arr a -> m ()


--------------------------------------------------------------------------------
-- Core layer
--------------------------------------------------------------------------------

instance ArrayAccess LocalArr CoreComp
  where
    writeArrAt offset spm range = CoreComp . singleInj . WriteArr offset spm range
    readArrAt offset spm range = CoreComp . singleInj . ReadArr offset spm range

instance ArrayAccess SharedArr CoreComp
  where
    writeArrAt offset spm range = CoreComp . singleInj . WriteArr offset spm range
    readArrAt offset spm range = CoreComp . singleInj . ReadArr offset spm range


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

onCore :: CoreId -> CoreComp () -> Host ()
onCore coreId = Host . singleInj . OnCore coreId


instance ArrayAccess LocalArr Host
  where
    writeArrAt offset spm range = Host . singleInj . WriteArr offset spm range
    readArrAt offset spm range = Host . singleInj . ReadArr offset spm range

instance ArrayAccess SharedArr Host
  where
    writeArrAt offset spm range = Host . singleInj . WriteArr offset spm range
    readArrAt offset spm range = Host . singleInj . ReadArr offset spm range


--------------------------------------------------------------------------------
-- Allocation layer
--------------------------------------------------------------------------------

allocLArr :: PrimType a => CoreId -> Size -> Multicore (LocalArr a)
allocLArr coreId = Multicore . singleInj . AllocLArr coreId

allocSArr :: PrimType a => Size -> Multicore (SharedArr a)
allocSArr = Multicore . singleInj . AllocSArr

onHost :: Host a -> Multicore a
onHost = Multicore . singleInj . OnHost
