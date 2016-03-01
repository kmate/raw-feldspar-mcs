module Feldspar.Multicore.Representation where

import Control.Monad.Identity
import Control.Monad.Operational.Higher
import Control.Monad.Trans

import Data.ALaCarte
import Data.Word

import Feldspar
import Feldspar.Representation
import Feldspar.Run.Frontend
import Feldspar.Run.Representation

import qualified Language.Embedded.Imperative as Imp
import qualified Language.Embedded.Imperative.CMD as Imp


type CoreId = Word32
type Size   = Word32
type Range  = (Data Index, Data Index)

data LocalArr a
    = LocalArr


data AllocCMD (prog :: * -> *) a
  where
    Alloc :: CoreId -> Size -> AllocCMD prog (LocalArr a)

instance HFunctor AllocCMD
  where
    hfmap _ (Alloc coreId size) = Alloc coreId size


data RunHostCMD (prog :: * -> *) a
  where
    RunHost :: Host a -> RunHostCMD prog a

instance HFunctor RunHostCMD
  where
    hfmap _ (RunHost host) = RunHost host


type AllocHostCMD
    =   AllocCMD
    :+: RunHostCMD

type AllocHost a = Program AllocHostCMD a


data MulticoreCMD (prog :: * -> *) a
  where
    Fetch  :: LocalArr a -> Range -> Arr a -> MulticoreCMD prog ()
    Flush  :: LocalArr a -> Range -> Arr a -> MulticoreCMD prog ()
    OnCore :: CoreId -> Comp () -> MulticoreCMD prog ()

instance HFunctor MulticoreCMD
  where
    hfmap _ (Fetch localArr range arr) = Fetch localArr range arr
    hfmap _ (Flush localArr range arr) = Flush localArr range arr
    hfmap _ (OnCore coreId coreComp)   = OnCore coreId coreComp


type HostCMD
    =   MulticoreCMD
    :+: RunCMD

newtype HostT m a = Host { unHost :: ProgramT HostCMD m a }
    deriving (Functor, Applicative, Monad, MonadTrans)

type Host = HostT Comp


type instance IExp (HostCMD)       = Data
type instance IExp (HostCMD :+: i) = Data

instance MonadComp m => MonadComp (HostT m) where
    liftComp        = lift . liftComp
    iff c t f       = Host $ Imp.iff c (unHost t) (unHost f)
    for  range body = Host $ Imp.for range (unHost . body)
    while cont body = Host $ Imp.while (unHost cont) (unHost body)

instance (a ~ ()) => PrintfType (HostT m a)
  where
    fprf h form = Host . singleE . Imp.FPrintf h form . reverse

-- FIXME: complete or remove MonadRun instance
instance MonadRun m => MonadRun (HostT m) where
    liftRun = undefined
