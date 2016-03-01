module Feldspar.Multicore.Frontend where

import Control.Monad.Operational.Higher

import Feldspar
import Feldspar.Multicore.Representation


alloc :: CoreId -> Size -> AllocHost (LocalArr a)
alloc coreId size = singleInj $ Alloc coreId size

runHost :: Host a -> AllocHost a
runHost = singleInj . RunHost


fetch :: LocalArr a -> Range -> Arr a -> Host ()
fetch localArr range = Host . singleInj . Fetch localArr range

flush :: LocalArr a -> Range -> Arr a -> Host ()
flush localArr range = Host . singleInj . Flush localArr range

onCore :: CoreId -> Comp () -> Host ()
onCore coreId = Host . singleInj . OnCore coreId
