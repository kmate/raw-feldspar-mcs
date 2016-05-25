module Feldspar.Multicore.Representation where

import Control.Monad.Operational.Higher
import Control.Monad.Trans
import Data.Word
import GHC.Exts (Constraint)

import Feldspar
import Feldspar.Primitive.Representation
import Feldspar.Representation
import Feldspar.Run
import Feldspar.Run.Concurrent
import Feldspar.Run.Representation

import qualified Language.Embedded.Concurrent.CMD as Imp
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

data BulkArrCMD (arr :: * -> *) fs a
  where
    WriteArr :: PrimType a
             => Data Index -> arr a
             -> IndexRange -> Arr a
             -> BulkArrCMD arr (Param3 (prog :: * -> *)
                                       (exp  :: * -> *)
                                       (pred :: * -> Constraint)) ()
    ReadArr  :: PrimType a
             => Data Index -> arr a
             -> IndexRange -> Arr a -> BulkArrCMD arr (Param3 prog exp pred) ()

instance HFunctor (BulkArrCMD arr)
  where
    hfmap _ (WriteArr offset spm range ram) = WriteArr offset spm range ram
    hfmap _ (ReadArr  offset spm range ram) = ReadArr  offset spm range ram

runBulkArrCMD :: (MonadComp m, ArrayWrapper arr)
              => BulkArrCMD arr (Param3 m exp pred) a -> m a
runBulkArrCMD (WriteArr offset spm (lower, upper) ram) =
    for (lower, 1, Incl upper) $ \i -> do
        item :: Data a <- getArr i ram
        setArr (i - lower + offset) item (unwrapArr spm)
runBulkArrCMD (ReadArr offset spm (lower, upper) ram) =
    for (lower, 1, Incl upper) $ \i -> do
        item :: Data a <- getArr (i - lower + offset) (unwrapArr spm)
        setArr i item ram


--------------------------------------------------------------------------------
-- Busy waiting
--------------------------------------------------------------------------------

data WaitCMD fs a
  where
    BusyWait :: WaitCMD (Param3 (prog :: * -> *)
                                (exp  :: * -> *)
                                (pred :: * -> Constraint)) ()

instance HFunctor WaitCMD
  where
    hfmap _ BusyWait = BusyWait

runWaitCMD :: WaitCMD (Param3 Run exp pred) a -> Run a
runWaitCMD BusyWait = delayThread (value 100 :: Data Int32)


--------------------------------------------------------------------------------
-- Halting core
--------------------------------------------------------------------------------

data CoreRef = CoreRefComp CoreId | CoreRefRun ThreadId

data HaltCMD fs a
  where
    HaltCore :: CoreRef -> HaltCMD (Param3 (prog :: * -> *)
                                   (exp  :: * -> *)
                                   (pred :: * -> Constraint)) ()

instance HFunctor HaltCMD
  where
    hfmap _ (HaltCore cr) = HaltCore cr

runHaltCMD :: HaltCMD (Param3 Run exp pred) a -> Run a
runHaltCMD (HaltCore (CoreRefRun t)) = killThread t


--------------------------------------------------------------------------------
-- Core layer
--------------------------------------------------------------------------------

type CoreCMD = Imp.ControlCMD
           :+: WaitCMD
           :+: HaltCMD
           :+: BulkArrCMD LocalArr
           :+: BulkArrCMD SharedArr

newtype CoreCompT m a = CoreComp
    { unCoreComp :: ProgramT CoreCMD (Param2 Data PrimType') m a }
  deriving (Functor, Applicative, Monad, MonadTrans)

type CoreComp = CoreCompT Comp

runCoreComp :: CoreComp a -> Run a
runCoreComp = interpretT (Run . lift . unComp) . unCoreComp


instance MonadComp CoreComp where
    liftComp        = lift . liftComp
    iff cond t f    = CoreComp $ Imp.iff cond (unCoreComp t) (unCoreComp f)
    for range body  = CoreComp $ Imp.for range (unCoreComp . body)
    while cont body = CoreComp $ Imp.while (unCoreComp cont) (unCoreComp body)


runControlCompCMD :: Imp.ControlCMD (Param3 Run Data PrimType') a -> Run a
runControlCompCMD (Imp.If cond t f)     = iff cond t f
runControlCompCMD (Imp.For range body)  = Run $ singleInj $ Imp.For range (unRun . body)
runControlCompCMD (Imp.While cond body) = while cond body

instance Interp WaitCMD Run (Param2 Data PrimType')
  where interp = runWaitCMD

instance Interp HaltCMD Run (Param2 Data PrimType')
  where interp = runHaltCMD


--------------------------------------------------------------------------------
-- Host layer
--------------------------------------------------------------------------------

data MulticoreCMD fs a
  where
    OnCore :: CoreId
           -> (CoreRef -> CoreComp ())
           -> MulticoreCMD (Param3 (prog :: * -> *)
                                   (exp  :: * -> *)
                                   (pred :: * -> Constraint)) CoreRef

instance HFunctor MulticoreCMD
  where
    hfmap _ (OnCore coreId comp) = OnCore coreId comp


type HostCMD = Imp.ControlCMD
           :+: Imp.ThreadCMD
           :+: WaitCMD
           :+: HaltCMD
           :+: BulkArrCMD LocalArr
           :+: BulkArrCMD SharedArr
           :+: MulticoreCMD

newtype HostT m a = Host
    { unHost :: ProgramT HostCMD (Param2 Data PrimType') m a }
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
    fprf h form = lift . Run . singleInj . Imp.FPrintf h form . reverse


runControlRunCMD :: Imp.ControlCMD (Param3 Run Data PrimType') a -> Run a
runControlRunCMD (Imp.If cond t f)     = iff cond t f
runControlRunCMD (Imp.For range body)  = Run $ singleInj $ Imp.For range (unRun . body)
runControlRunCMD (Imp.While cond body) = while cond body

instance Interp Imp.ControlCMD Run (Param2 Data PrimType')
  where interp = runControlRunCMD


runThreadCMD :: Imp.ThreadCMD (Param3 Run Data PrimType') a -> Run a
runThreadCMD (Imp.ForkWithId p) = forkWithId p

instance Interp Imp.ThreadCMD Run (Param2 Data PrimType')
  where interp = runThreadCMD


instance ArrayWrapper arr => Interp (BulkArrCMD arr) Run (Param2 exp pred)
  where interp = runBulkArrCMD


runMulticoreCMD :: MulticoreCMD (Param3 Run exp pred) a -> Run a
runMulticoreCMD (OnCore coreId comp) =
    (CoreRefRun <$>) . forkWithId $ liftRun . runCoreComp . comp . CoreRefRun


instance Interp MulticoreCMD Run (Param2 exp pred)
  where interp = runMulticoreCMD


--------------------------------------------------------------------------------
-- Allocation layer
--------------------------------------------------------------------------------

data AllocCMD fs a
  where
    AllocLArr :: pred a
              => CoreId
              -> Size
              -> AllocCMD (Param3 (prog :: * -> *)
                                  (exp  :: * -> *)
                                  (pred :: * -> Constraint)) (LocalArr a)
    AllocSArr :: pred a
              => Size
              -> AllocCMD (Param3 prog exp pred) (SharedArr a)
    OnHost :: Host a -> AllocCMD (Param3 prog exp pred) a

instance HFunctor AllocCMD
  where
    hfmap _ (AllocLArr coreId size) = AllocLArr coreId size
    hfmap _ (AllocSArr size)        = AllocSArr size
    hfmap _ (OnHost host)           = OnHost host


newtype Multicore a = Multicore
    { unMulticore :: Program AllocCMD (Param2 Prim PrimType) a }
  deriving (Functor, Applicative, Monad)


runAllocCMD :: AllocCMD (Param3 Run Prim PrimType) a -> Run a
runAllocCMD (AllocLArr _ size) = LocalArr  <$> newArr (value size)
runAllocCMD (AllocSArr   size) = SharedArr <$> newArr (value size)
runAllocCMD (OnHost host)      = runHost host

instance Interp AllocCMD Run (Param2 Prim PrimType)
  where interp = runAllocCMD

instance MonadRun Multicore where liftRun = interpret . unMulticore
