module Feldspar.Multicore.Representation where

import Control.Monad.Operational.Higher
import Control.Monad.Trans
import Data.Word

import Feldspar
import Feldspar.Run
import Feldspar.Run.Concurrent
import Feldspar.Run.Representation

import qualified Language.Embedded.CExp as Exp
import qualified Language.Embedded.Expression as Exp
import qualified Language.Embedded.Imperative as Imp
import qualified Language.Embedded.Imperative.CMD as Imp


type CoreId     = Word32
type Size       = Word32
type IndexRange = (Data Index, Data Index)


--------------------------------------------------------------------------------
-- Host layer
--------------------------------------------------------------------------------

data HostCMD (prog :: * -> *) a
  where
    Fetch  :: SmallType a => Arr a -> IndexRange -> Arr a -> HostCMD prog ()
    Flush  :: SmallType a => Arr a -> IndexRange -> Arr a -> HostCMD prog ()
    OnCore :: CoreId -> Comp () -> HostCMD prog ()

instance HFunctor HostCMD
  where
    hfmap _ (Fetch spm range ram) = Fetch spm range ram
    hfmap _ (Flush spm range ram) = Flush spm range ram
    hfmap _ (OnCore coreId comp)  = OnCore coreId comp


newtype HostT m a = Host { unHost :: ProgramT HostCMD m a }
  deriving (Functor, Applicative, Monad, MonadTrans)

type Host = HostT Run

runHost :: Host a -> Run a
runHost = interpretT id . unHost


instance MonadComp Host where
    liftComp        = lift . liftComp
    iff cond t f    = lift $ iff cond (runHost t) (runHost f)
    for range body  = lift $ for range (runHost . body)
    while cont body = lift $ while (runHost cont) (runHost body)


type instance IExp HostCMD         = Data
type instance IExp (HostCMD :+: i) = Data

instance (a ~ ()) => PrintfType (Host a)
  where
    fprf h form = lift . Run . singleE . Imp.FPrintf h form . reverse


runHostCMD :: HostCMD Run a -> Run a
runHostCMD (Fetch spm (lower, upper) ram) =
    for (lower, 1, Incl upper) $ \i -> do
        item :: Data a <- getArr i ram
        setArr (i - lower) item spm
runHostCMD (Flush spm (lower, upper) ram) =
    for (lower, 1, Incl upper) $ \i -> do
        item :: Data a <- getArr (i - lower) spm
        setArr i item ram
runHostCMD (OnCore coreId comp) = void $ fork $liftRun comp

instance Interp HostCMD Run where interp = runHostCMD


--------------------------------------------------------------------------------
-- Allocation layer
--------------------------------------------------------------------------------

data AllocHostCMD exp (prog :: * -> *) a
  where
    Alloc  :: (Exp.VarPred exp a, SmallType a) => CoreId -> Size -> AllocHostCMD exp prog (Arr a)
    OnHost :: Host a -> AllocHostCMD exp prog a

instance HFunctor (AllocHostCMD exp)
  where
    hfmap _ (Alloc coreId size) = Alloc coreId size
    hfmap _ (OnHost host)       = OnHost host

type instance IExp (AllocHostCMD e)       = e
type instance IExp (AllocHostCMD e :+: i) = e

newtype AllocHost a = AllocHost { unAllocHost :: Program (AllocHostCMD Exp.CExp) a }
  deriving (Functor, Applicative, Monad)


runAllocHostCMD :: (AllocHostCMD exp) Run a -> Run a
runAllocHostCMD (Alloc coreId size) = newArr (value size)
runAllocHostCMD (OnHost host)       = runHost host

instance Interp (AllocHostCMD exp) Run where interp = runAllocHostCMD

instance MonadRun AllocHost where liftRun = interpret . unAllocHost
