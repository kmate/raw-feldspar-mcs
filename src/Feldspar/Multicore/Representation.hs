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


sharedId :: CoreId
sharedId = maxBound  -- use the maximum representable value for shared addresses


newtype LocalArr  a = LocalArr  { unLocalArr  :: Arr a }
newtype SharedArr a = SharedArr { unSharedArr :: Arr a }

class ArrayWrapper arr
  where
    wrapArr   :: Arr a -> arr a
    unwrapArr :: arr a -> Arr a

instance ArrayWrapper LocalArr
  where
    wrapArr   = LocalArr
    unwrapArr = unLocalArr

instance ArrayWrapper SharedArr
  where
    wrapArr   = SharedArr
    unwrapArr = unSharedArr


--------------------------------------------------------------------------------
-- Bulk array commands and interpretation
--------------------------------------------------------------------------------

data BulkArrCMD (arr :: * -> *) (prog :: * -> *) a
  where
    WriteArr :: SmallType a
             => Data Index -> arr a
             -> IndexRange -> Arr a -> BulkArrCMD arr prog ()
    ReadArr  :: SmallType a
             => Data Index -> arr a
             -> IndexRange -> Arr a -> BulkArrCMD arr prog ()

instance HFunctor (BulkArrCMD arr)
  where
    hfmap _ (WriteArr offset spm range ram) = WriteArr offset spm range ram
    hfmap _ (ReadArr  offset spm range ram) = ReadArr  offset spm range ram


runBulkArrCMD :: MonadComp m => ArrayWrapper arr => BulkArrCMD arr m a -> m a
runBulkArrCMD (WriteArr offset spm (lower, upper) ram) =
    for (lower, 1, Incl upper) $ \i -> do
        item :: Data a <- getArr i ram
        setArr (i - lower + offset) item (unwrapArr spm)
runBulkArrCMD (ReadArr offset spm (lower, upper) ram) =
    for (lower, 1, Incl upper) $ \i -> do
        item :: Data a <- getArr (i - lower + offset) (unwrapArr spm)
        setArr i item ram


--------------------------------------------------------------------------------
-- Core layer
--------------------------------------------------------------------------------

type CoreCMD = Imp.ControlCMD Data
           :+: BulkArrCMD LocalArr
           :+: BulkArrCMD SharedArr

newtype CoreCompT m a = CoreComp { unCoreComp :: ProgramT CoreCMD m a }
  deriving (Functor, Applicative, Monad, MonadTrans)

type CoreComp = CoreCompT Comp

runCoreComp :: CoreComp a -> Comp a
runCoreComp = interpretT id . unCoreComp


instance MonadComp CoreComp where
    liftComp        = lift . liftComp
    iff cond t f    = CoreComp $ Imp.iff cond (unCoreComp t) (unCoreComp f)
    for range body  = CoreComp $ Imp.for range (unCoreComp . body)
    while cont body = CoreComp $ Imp.while (unCoreComp cont) (unCoreComp body)


runControlCMD :: MonadComp m => (Imp.ControlCMD Data) m a -> m a
runControlCMD (Imp.If cond t f)     = iff cond t f
runControlCMD (Imp.For range body)  = for range body
runControlCMD (Imp.While cond body) = while cond body

instance Interp (Imp.ControlCMD Data) Comp where interp = runControlCMD

instance (ArrayWrapper arr) => Interp (BulkArrCMD arr) Comp where interp = runBulkArrCMD


--------------------------------------------------------------------------------
-- Host layer
--------------------------------------------------------------------------------

data MulticoreCMD (prog :: * -> *) a
  where
    OnCore :: CoreId -> CoreComp () -> MulticoreCMD prog ()

instance HFunctor MulticoreCMD
  where
    hfmap _ (OnCore coreId comp) = OnCore coreId comp


type HostCMD = Imp.ControlCMD Data
           :+: BulkArrCMD LocalArr
           :+: BulkArrCMD SharedArr
           :+: MulticoreCMD

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


instance (a ~ ()) => PrintfType (Host a)
  where
    fprf h form = lift . Run . singleE . Imp.FPrintf h form . reverse


instance Interp (Imp.ControlCMD Data) Run where interp = runControlCMD

instance ArrayWrapper arr => Interp (BulkArrCMD arr) Run where interp = runBulkArrCMD


runMulticoreCMD :: MulticoreCMD Run a -> Run a
runMulticoreCMD (OnCore coreId comp) = void $ fork $ liftRun $ runCoreComp comp

instance Interp MulticoreCMD Run where interp = runMulticoreCMD


--------------------------------------------------------------------------------
-- Allocation layer
--------------------------------------------------------------------------------

data AllocCMD exp (prog :: * -> *) a
  where
    AllocLArr :: (Exp.VarPred exp a, SmallType a)
              => CoreId -> Size -> AllocCMD exp prog (LocalArr a)
    AllocSArr :: (Exp.VarPred exp a, SmallType a)
              => Size -> AllocCMD exp prog (SharedArr a)
    OnHost :: Host a -> AllocCMD exp prog a

instance HFunctor (AllocCMD exp)
  where
    hfmap _ (AllocLArr coreId size) = AllocLArr coreId size
    hfmap _ (AllocSArr size)        = AllocSArr size
    hfmap _ (OnHost host)           = OnHost host

type instance IExp (AllocCMD e)       = e
type instance IExp (AllocCMD e :+: i) = e

newtype Multicore a = Multicore { unMulticore :: Program (AllocCMD Exp.CExp) a }
  deriving (Functor, Applicative, Monad)


runAllocCMD :: (AllocCMD exp) Run a -> Run a
runAllocCMD (AllocLArr _ size) = LocalArr  <$> newArr (value size)
runAllocCMD (AllocSArr   size) = SharedArr <$> newArr (value size)
runAllocCMD (OnHost host)      = runHost host

instance Interp (AllocCMD exp) Run where interp = runAllocCMD

instance MonadRun Multicore where liftRun = interpret . unMulticore
