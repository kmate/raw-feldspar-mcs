module Feldspar.Multicore.Frontend where

import Control.Monad.Operational.Higher
import GHC.TypeLits

import Feldspar
import Feldspar.Multicore.Representation


--------------------------------------------------------------------------------
-- Core layer
--------------------------------------------------------------------------------

local :: LocalArr ca a -> CoreComp cb (Arr a)
local = return . unLocalArr

(-<) :: (Arr a -> CoreComp cb b) -> LocalArr ca a -> CoreComp cb b
action -< arr = action =<< local arr

infixr 1 -<

forever :: KnownNat coreId => CoreComp coreId () -> CoreComp coreId ()
forever = while (return $ true)


--------------------------------------------------------------------------------
-- Host layer
--------------------------------------------------------------------------------

writeArr :: (KnownNat coreId, SmallType a)
         => LocalArr coreId a -> IndexRange -> Arr a -> Host ()
writeArr = writeArrAt 0

readArr :: (KnownNat coreId, SmallType a)
        => LocalArr coreId a -> IndexRange -> Arr a -> Host ()
readArr = readArrAt 0

writeArrAt :: (KnownNat coreId, SmallType a)
           => Data Index -> LocalArr coreId a
           -> IndexRange -> Arr a -> Host ()
writeArrAt offset spm range = Host . singleInj . WriteArr offset spm range

readArrAt :: (KnownNat coreId, SmallType a)
          => Data Index -> LocalArr coreId a
          -> IndexRange -> Arr a -> Host ()
readArrAt offset spm range = Host . singleInj . ReadArr offset spm range

onCore :: KnownNat coreId => CoreComp coreId () -> Host ()
onCore = Host . singleInj . OnCore


--------------------------------------------------------------------------------
-- Allocation layer
--------------------------------------------------------------------------------

allocArr :: (KnownNat coreId, SmallType a)
      => Size -> Multicore (LocalArr coreId a)
allocArr = Multicore . singleE . AllocArr

onHost :: Host a -> Multicore a
onHost = Multicore . singleE . OnHost
