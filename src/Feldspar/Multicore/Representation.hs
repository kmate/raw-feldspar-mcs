{-# LANGUAGE CPP #-}
module Feldspar.Multicore.Representation where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
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

data MulticoreCMD (prog :: * -> *) a
  where
    Fetch  :: (KnownNat coreId, SmallType a)
           => Data Index -> LocalArr coreId a -> IndexRange -> Arr a -> MulticoreCMD prog ()
    Flush  :: (KnownNat coreId, SmallType a)
           => Data Index -> LocalArr coreId a -> IndexRange -> Arr a -> MulticoreCMD prog ()
    OnCore :: KnownNat coreId => CoreComp coreId () -> MulticoreCMD prog ()

instance HFunctor MulticoreCMD
  where
    hfmap _ (Fetch spm offset range ram) = Fetch spm offset range ram
    hfmap _ (Flush spm offset range ram) = Flush spm offset range ram
    hfmap _ (OnCore comp)                = OnCore comp


type HostCMD = MulticoreCMD :+: Imp.ControlCMD Data

newtype HostT m a = Host { unHost :: ProgramT HostCMD m a }
  deriving (Functor, Applicative, Monad, MonadTrans)

type Host = HostT Run

runHost :: Host a -> Run a
runHost = interpretT id . unHost


instance MonadComp Host where
    liftComp        = lift . liftComp
    iff cond t f    = Host $ Imp.iff cond (unHost t) (unHost f)
    for range body  = Host $ Imp.for range (unHost . body)
    while cont body = Host $ Imp.while (unHost cont) (unHost body)


type instance IExp HostCMD         = Data
type instance IExp (HostCMD :+: i) = Data

instance (a ~ ()) => PrintfType (Host a)
  where
    fprf h form = lift . Run . singleE . Imp.FPrintf h form . reverse


runControlCMD :: (Imp.ControlCMD Data) Run a -> Run a
runControlCMD (Imp.If cond t f)     = iff cond t f
runControlCMD (Imp.For range body)  = for range body
runControlCMD (Imp.While cond body) = while cond body

instance Interp (Imp.ControlCMD Data) Run where interp = runControlCMD


runMulticoreCMD :: MulticoreCMD Run a -> Run a
runMulticoreCMD (Fetch spm offset (lower, upper) ram) =
    for (lower, 1, Incl upper) $ \i -> do
        item :: Data a <- getArr i ram
        setArr (i - lower + offset) item (unLocalArr spm)
runMulticoreCMD (Flush spm offset (lower, upper) ram) =
    for (lower, 1, Incl upper) $ \i -> do
        item :: Data a <- getArr (i - lower + offset) (unLocalArr spm)
        setArr i item ram
runMulticoreCMD (OnCore comp) = void $ fork $ liftRun $ unCoreComp comp

instance Interp MulticoreCMD Run where interp = runMulticoreCMD


--------------------------------------------------------------------------------
-- Allocation layer
--------------------------------------------------------------------------------

data AllocCMD exp (prog :: * -> *) a
  where
    Alloc  :: (KnownNat coreId, Exp.VarPred exp a, SmallType a)
           => Size -> AllocCMD exp prog (LocalArr coreId a)
    OnHost :: Host a -> AllocCMD exp prog a

instance HFunctor (AllocCMD exp)
  where
    hfmap _ (Alloc size)  = Alloc size
    hfmap _ (OnHost host) = OnHost host

type instance IExp (AllocCMD e)       = e
type instance IExp (AllocCMD e :+: i) = e

newtype Multicore a = Multicore { unMulticore :: Program (AllocCMD Exp.CExp) a }
  deriving (Functor, Applicative, Monad)


runAllocHostCMD :: (AllocCMD exp) Run a -> Run a
runAllocHostCMD (Alloc size)  = LocalArr <$> newArr (value size)
runAllocHostCMD (OnHost host) = runHost host

instance Interp (AllocCMD exp) Run where interp = runAllocCMD

instance MonadRun Multicore where liftRun = interpret . unMulticore
