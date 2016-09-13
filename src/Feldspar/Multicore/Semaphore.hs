module Feldspar.Multicore.Semaphore where

import Prelude hiding (not)

import Feldspar
import Feldspar.Multicore.CoreId
import Feldspar.Multicore.Reference
import Feldspar.Multicore.Representation


type Sem = DLRef Bool

allocSem :: CoreId -> Multicore Sem
allocSem = allocLRef

initSem :: (MonadComp m, LocalRefAccess m) => Sem -> m ()
initSem s = setLRef s false

acquire :: (MonadComp m, LocalRefAccess m) => Sem -> m ()
acquire s = do
    while (not <$> getLRef s) $ return ()
    initSem s

release :: (MonadComp m, LocalRefAccess m) => Sem -> m ()
release s = setLRef s true
