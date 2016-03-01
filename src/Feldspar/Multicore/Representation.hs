module Feldspar.Multicore.Representation where

import Control.Monad.Operational.Higher
import Control.Monad.Trans

import Data.ALaCarte
import Data.Word

import Feldspar
import Feldspar.Representation
import Feldspar.Run.Frontend
import Feldspar.Run.Representation

import GHC.TypeLits

import qualified Language.Embedded.Imperative as Imp
import qualified Language.Embedded.Imperative.CMD as Imp


-- NOTE: this shoud be used instead of Nat when GHC promotes kind synonyms
-- type CoreId = Nat
type Size   = Word32
type Range  = (Data Index, Data Index)

data LocalArr (coreId :: Nat) a
    = LocalArr


--------------------------------------------------------------------------------
-- Core layer
--------------------------------------------------------------------------------

data LocalArrCMD (prog :: * -> *) a
  where
    GetLArr       :: Data Index -> LocalArr coreId a -> LocalArrCMD prog a
    UnsafeGetLArr :: Data Index -> LocalArr coreId a -> LocalArrCMD prog a
    SetLArr       :: Data Index -> a -> LocalArr coreId a -> LocalArrCMD prog ()

instance HFunctor LocalArrCMD
  where
    hfmap _ (GetLArr       i arr)   = GetLArr i arr
    hfmap _ (UnsafeGetLArr i arr)   = UnsafeGetLArr i arr
    hfmap _ (SetLArr       i v arr) = SetLArr i v arr


type CoreCompCMD
    =   LocalArrCMD
    :+: CompCMD

newtype CoreComp (coreId :: Nat) a
    = CoreComp { unCoreComp :: ProgramT CoreCompCMD Comp a }
        deriving (Functor, Applicative, Monad)


type instance IExp (CoreCompCMD)       = Data
type instance IExp (CoreCompCMD :+: i) = Data

instance MonadComp (CoreComp coreId)
  where
    liftComp        = CoreComp . lift
    iff c t f       = CoreComp $ Imp.iff c (unCoreComp t) (unCoreComp f)
    for  range body = CoreComp $ Imp.for range (unCoreComp . body)
    while cont body = CoreComp $ Imp.while (unCoreComp cont) (unCoreComp body)


--------------------------------------------------------------------------------
-- Host layer
--------------------------------------------------------------------------------

data MulticoreCMD (prog :: * -> *) a
  where
    Fetch  :: LocalArr coreId a -> Range -> Arr a -> MulticoreCMD prog ()
    Flush  :: LocalArr coreId a -> Range -> Arr a -> MulticoreCMD prog ()
    OnCore :: MonadComp m => m () -> MulticoreCMD prog ()

instance HFunctor MulticoreCMD
  where
    hfmap _ (Fetch localArr range arr) = Fetch localArr range arr
    hfmap _ (Flush localArr range arr) = Flush localArr range arr
    hfmap _ (OnCore coreComp)   = OnCore coreComp


type HostCMD
    =   MulticoreCMD
    :+: RunCMD

-- FIXME: remove the constant below and hide the core id parameter
newtype Host a = Host { unHost :: ProgramT HostCMD (CoreComp 42) a }
  deriving (Functor, Applicative, Monad)


type instance IExp (HostCMD)       = Data
type instance IExp (HostCMD :+: i) = Data

instance MonadComp Host where
    liftComp        = Host . lift . liftComp
    iff c t f       = Host $ Imp.iff c (unHost t) (unHost f)
    for  range body = Host $ Imp.for range (unHost . body)
    while cont body = Host $ Imp.while (unHost cont) (unHost body)

instance (a ~ ()) => PrintfType (Host a)
  where
    fprf h form = Host . singleE . Imp.FPrintf h form . reverse

-- FIXME: complete or remove MonadRun instance
instance MonadRun Host where
    liftRun = undefined


--------------------------------------------------------------------------------
-- Allocation layer
--------------------------------------------------------------------------------

data AllocCMD (prog :: * -> *) a
  where
    Alloc :: Size -> AllocCMD prog (LocalArr coreId a)

instance HFunctor AllocCMD
  where
    hfmap _ (Alloc size) = Alloc size


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
