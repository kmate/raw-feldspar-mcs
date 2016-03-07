module Feldspar.Multicore.Representation where

import Control.Monad.Operational.Higher
import Control.Monad.Trans

import Data.Word

import Feldspar
import Feldspar.Run
import Feldspar.Run.Concurrent
import Feldspar.Run.Representation

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
    Wrap   :: Run a -> HostCMD prog a
    Fetch  :: Type a => Arr a -> IndexRange -> Arr a -> HostCMD prog ()
    Flush  :: Type a => Arr a -> IndexRange -> Arr a -> HostCMD prog ()
    OnCore :: CoreId -> Comp () -> HostCMD prog ()

instance HFunctor HostCMD
  where
    hfmap _ (Fetch dst range src) = Fetch dst range src
    hfmap _ (Flush src range dst) = Flush src range dst
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


type instance IExp (HostCMD)       = Data
type instance IExp (HostCMD :+: i) = Data

instance (a ~ ()) => PrintfType (Host a)
  where
    fprf h form = lift . Run . singleE . Imp.FPrintf h form . reverse


runHostCMD :: HostCMD Run a -> Run a
runHostCMD (Fetch dst (lower, upper) src) =
    for (lower, 1, Incl upper) $ \i -> do
        item :: Data a <- getArr i src
        setArr (i - lower) item dst
runHostCMD (Flush src (lower, upper) dst) =
    for (lower, 1, Incl upper) $ \i -> do
        item :: Data a <- getArr (i - lower) src
        setArr i item dst
runHostCMD (OnCore coreId comp) = fork (liftRun comp) >> return ()

instance Interp HostCMD Run where interp = runHostCMD

--------------------------------------------------------------------------------
-- Allocation layer
--------------------------------------------------------------------------------

data AllocHostCMD (prog :: * -> *) a
  where
    Alloc  :: Type a => CoreId -> Size -> AllocHostCMD prog (Arr a)
    OnHost :: Host a -> AllocHostCMD prog a

instance HFunctor AllocHostCMD
  where
    hfmap _ (Alloc coreId size) = Alloc coreId size
    hfmap _ (OnHost host)       = OnHost host

type AllocHost a = Program AllocHostCMD a


runAllocHostCMD :: AllocHostCMD IO a -> IO a
runAllocHostCMD (Alloc coreId size) = (runIO :: Run a -> IO a) (newArr (value size))
runAllocHostCMD (OnHost host)       = runIO $ runHost host

instance Interp AllocHostCMD IO where interp = runAllocHostCMD
