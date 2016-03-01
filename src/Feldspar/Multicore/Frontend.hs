module Feldspar.Multicore.Frontend where

import Control.Monad.Operational.Higher

import Feldspar
import Feldspar.Multicore.Representation


-- TODO: do we need to add constraints on `a` below?

alloc :: CoreId -> Size -> AllocHost (LocalArr a)
alloc coreId size = singleInj $ Alloc coreId size

runHost :: Host a -> AllocHost a
runHost = singleInj . RunHost

fetch :: LocalArr a -> Range -> Arr a -> Host ()
fetch localArr range = Host . singleInj . Fetch localArr range

flush :: LocalArr a -> Range -> Arr a -> Host ()
flush localArr range = Host . singleInj . Flush localArr range

onCore :: MonadComp m => CoreId -> m () -> Host ()
onCore coreId = Host . singleInj . OnCore coreId


-- TODO: implement these functions

getLArr :: (Syntax a, MonadComp m) => Data Index -> LocalArr (Internal a) -> m a
getLArr i = undefined

unsafeGetLArr :: (Syntax a, MonadComp m) => Data Index -> LocalArr (Internal a) -> m a
unsafeGetLArr i = undefined

setLArr :: forall m a . (Syntax a, MonadComp m) =>
    Data Index -> a -> LocalArr (Internal a) -> m ()
setLArr = undefined
