module Feldspar.Multicore.Representation where

import Control.Monad.Operational.Higher
import Control.Monad.Trans

import Data.ALaCarte
import Data.Array.IO
import Data.Typeable
import Data.Word

import Feldspar
import Feldspar.Representation
import Feldspar.Run.Frontend
import Feldspar.Run.Representation

import Language.Embedded.Expression
import qualified Language.Embedded.Imperative as Imp
import qualified Language.Embedded.Imperative.CMD as Imp


type CoreId = Word32
type Size   = Word32
type Range i = (i, i)


--------------------------------------------------------------------------------
-- Core layer
--------------------------------------------------------------------------------

data LocalArr i a
    = LocalArrComp VarId
    | LocalArrEval (IOArray i a)
  deriving Typeable


data LocalArrCMD exp (prog :: * -> *) a
  where
    GetLArr :: (VarPred exp a, VarPred exp i, Integral i, Ix i) =>
               exp i -> LocalArr i a -> LocalArrCMD exp prog (exp a)
    SetLArr :: (VarPred exp a, VarPred exp i, Integral i, Ix i) =>
               exp i -> exp a -> LocalArr i a -> LocalArrCMD exp prog ()

instance HFunctor (LocalArrCMD exp)
  where
    hfmap _ (GetLArr i arr)   = GetLArr i arr
    hfmap _ (SetLArr i v arr) = SetLArr i v arr


type CoreCompCMD
    =   LocalArrCMD Data
    :+: CompCMD

newtype CoreComp a = CoreComp { unCoreComp :: ProgramT CoreCompCMD Comp a }
  deriving (Functor, Applicative, Monad)


type instance IExp (CoreCompCMD)       = Data
type instance IExp (CoreCompCMD :+: i) = Data

instance MonadComp CoreComp
  where
    liftComp        = CoreComp . lift
    iff c t f       = CoreComp $ Imp.iff c (unCoreComp t) (unCoreComp f)
    for  range body = CoreComp $ Imp.for range (unCoreComp . body)
    while cont body = CoreComp $ Imp.while (unCoreComp cont) (unCoreComp body)


--------------------------------------------------------------------------------
-- Host layer
--------------------------------------------------------------------------------

data MulticoreCMD exp (prog :: * -> *) a
  where
    Fetch  :: (VarPred exp a, VarPred exp i, Integral i, Ix i) =>
              LocalArr i a -> Range i -> Arr a -> MulticoreCMD exp prog ()
    Flush  :: (VarPred exp a, VarPred exp i, Integral i, Ix i) =>
              LocalArr i a -> Range i -> Arr a -> MulticoreCMD exp prog ()
    OnCore :: MonadComp m => CoreId -> m () -> MulticoreCMD exp prog ()

instance HFunctor (MulticoreCMD exp)
  where
    hfmap _ (Fetch localArr range arr) = Fetch localArr range arr
    hfmap _ (Flush localArr range arr) = Flush localArr range arr
    hfmap _ (OnCore coreId coreComp)   = OnCore coreId coreComp


type HostCMD
    =   MulticoreCMD Data
    :+: RunCMD

newtype Host a = Host { unHost :: ProgramT HostCMD CoreComp a }
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

data AllocCMD exp (prog :: * -> *) a
  where
    Alloc :: (VarPred exp a, VarPred exp i, Integral i, Ix i) =>
             CoreId -> Size -> AllocCMD exp prog (LocalArr i a)

instance HFunctor (AllocCMD exp)
  where
    hfmap _ (Alloc coreId size) = Alloc coreId size


data RunHostCMD (prog :: * -> *) a
  where
    RunHost :: Host a -> RunHostCMD prog a

instance HFunctor RunHostCMD
  where
    hfmap _ (RunHost host) = RunHost host


type AllocHostCMD
    =   AllocCMD Data
    :+: RunHostCMD

type AllocHost a = Program AllocHostCMD a

type instance IExp (AllocHostCMD)       = Data
type instance IExp (AllocHostCMD :+: i) = Data
