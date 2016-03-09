module Feldspar.Multicore.Representation where

import Control.Monad.Operational.Higher
import Control.Monad.Trans
import Data.Word
import GHC.TypeLits

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
-- Core layer
--------------------------------------------------------------------------------

newtype LocalArr (coreId :: Nat) a = LocalArr { unLocalArr :: Arr a }

newtype CoreComp (coreId :: Nat) a = CoreComp { unCoreComp :: Comp a }
  deriving (Functor, Applicative, Monad)

instance KnownNat coreId => MonadComp (CoreComp coreId)
  where
    liftComp        = CoreComp . liftComp
    iff cond t f    = CoreComp $ iff cond (unCoreComp t) (unCoreComp f)
    for range body  = CoreComp $ for range (unCoreComp . body)
    while cont body = CoreComp $ while (unCoreComp cont) (unCoreComp body)

--------------------------------------------------------------------------------
-- Host layer
--------------------------------------------------------------------------------

data HostCMD (prog :: * -> *) a
  where
    Fetch  :: (KnownNat coreId, SmallType a)
           => LocalArr coreId a -> IndexRange -> Arr a -> HostCMD prog ()
    Flush  :: (KnownNat coreId, SmallType a)
           => LocalArr coreId a -> IndexRange -> Arr a -> HostCMD prog ()
    OnCore :: KnownNat coreId => CoreComp coreId () -> HostCMD prog ()

instance HFunctor HostCMD
  where
    hfmap _ (Fetch dst range src) = Fetch dst range src
    hfmap _ (Flush src range dst) = Flush src range dst
    hfmap _ (OnCore comp)         = OnCore comp


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
runHostCMD (Fetch dst (lower, upper) src) =
    for (lower, 1, Incl upper) $ \i -> do
        item :: Data a <- getArr i src
        setArr (i - lower) item (unLocalArr dst)
runHostCMD (Flush src (lower, upper) dst) =
    for (lower, 1, Incl upper) $ \i -> do
        item :: Data a <- getArr (i - lower) (unLocalArr src)
        setArr i item dst
runHostCMD (OnCore comp) = void $ fork $ liftRun $ unCoreComp comp

instance Interp HostCMD Run where interp = runHostCMD


--------------------------------------------------------------------------------
-- Allocation layer
--------------------------------------------------------------------------------

data AllocHostCMD exp (prog :: * -> *) a
  where
    Alloc  :: (KnownNat coreId, Exp.VarPred exp a, SmallType a)
           => Size -> AllocHostCMD exp prog (LocalArr coreId a)
    OnHost :: Host a -> AllocHostCMD exp prog a

instance HFunctor (AllocHostCMD exp)
  where
    hfmap _ (Alloc size)  = Alloc size
    hfmap _ (OnHost host) = OnHost host

type instance IExp (AllocHostCMD e)       = e
type instance IExp (AllocHostCMD e :+: i) = e

newtype AllocHost a = AllocHost { unAllocHost :: Program (AllocHostCMD Exp.CExp) a }
  deriving (Functor, Applicative, Monad)


runAllocHostCMD :: (AllocHostCMD exp) Run a -> Run a
runAllocHostCMD (Alloc size)  = fmap LocalArr $ newArr (value size)
runAllocHostCMD (OnHost host) = runHost host

instance Interp (AllocHostCMD exp) Run where interp = runAllocHostCMD

instance MonadRun AllocHost where liftRun = interpret . unAllocHost
