module Feldspar.Multicore.Representation where

import Control.Monad.Operational.Higher
import Control.Monad.Trans
import Data.Word

import Feldspar
import Feldspar.Multicore.CoreId
import Feldspar.Multicore.Channel.Representation hiding (CoreId)
import Feldspar.Primitive.Representation
import Feldspar.Representation
import Feldspar.Run
import Feldspar.Run.Concurrent
import Feldspar.Run.Representation

import qualified Language.Embedded.Concurrent.CMD as Imp
import qualified Language.Embedded.Imperative as Imp
import qualified Language.Embedded.Imperative.CMD as Imp


newtype LArr a = LArr { unLArr :: Arr a }
newtype SArr a = SArr { unSArr :: Arr a }

type DLArr  a = LArr (Data a)
type DSArr a = SArr (Data a)

class ArrayWrapper arr
  where
    wrapArr   :: Arr a -> arr a
    unwrapArr :: arr a -> Arr a

instance ArrayWrapper LArr
  where
    wrapArr   = LArr
    unwrapArr = unLArr

instance ArrayWrapper SArr
  where
    wrapArr   = SArr
    unwrapArr = unSArr


--------------------------------------------------------------------------------
-- Bulk array commands and interpretation
--------------------------------------------------------------------------------

type IndexRange = (Data Index, Data Index)

data BulkArrCMD (arr :: * -> *) fs a
  where
    WriteArr :: PrimType a
             => Data Index -> arr (Data a)
             -> IndexRange -> DArr a
             -> BulkArrCMD arr (Param3 prog exp pred) ()
    ReadArr  :: PrimType a
             => Data Index -> arr (Data a)
             -> IndexRange -> DArr a
             -> BulkArrCMD arr (Param3 prog exp pred) ()

instance HFunctor (BulkArrCMD arr)
  where
    hfmap _ (WriteArr offset spm range ram) = WriteArr offset spm range ram
    hfmap _ (ReadArr  offset spm range ram) = ReadArr  offset spm range ram

runBulkArrCMD :: (MonadComp m, ArrayWrapper arr)
              => BulkArrCMD arr (Param3 m exp pred) a -> m a
runBulkArrCMD (WriteArr offset spm (lower, upper) ram) =
    for (lower, 1, Incl upper) $ \i -> do
        item <- getArr ram i
        setArr (unwrapArr spm) (i - lower + offset) item
runBulkArrCMD (ReadArr offset spm (lower, upper) ram) =
    for (lower, 1, Incl upper) $ \i -> do
        item <- getArr (unwrapArr spm) (i - lower + offset)
        setArr ram i item


--------------------------------------------------------------------------------
-- Halting core
--------------------------------------------------------------------------------

data CoreRef = CoreRefComp CoreId | CoreRefRun ThreadId

data CoreHaltCMD fs a
  where
    HaltCore :: CoreRef -> CoreHaltCMD (Param3 prog exp pred) ()

instance HFunctor CoreHaltCMD
  where
    hfmap _ (HaltCore cr) = HaltCore cr

runCoreHaltCMD :: CoreHaltCMD (Param3 Run exp pred) a -> Run a
runCoreHaltCMD (HaltCore (CoreRefRun t)) = killThread t


--------------------------------------------------------------------------------
-- Core layer
--------------------------------------------------------------------------------

type CoreCMD = Imp.ControlCMD
           :+: CoreHaltCMD
           :+: CoreChanCMD
           :+: BulkArrCMD LArr
           :+: BulkArrCMD SArr

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

instance Interp CoreHaltCMD Run (Param2 Data PrimType')
  where interp = runCoreHaltCMD


--------------------------------------------------------------------------------
-- Host layer
--------------------------------------------------------------------------------

data MulticoreCMD fs a
  where
    OnCore :: CoreId
           -> (CoreRef -> CoreComp ())
           -> MulticoreCMD (Param3 prog exp pred) CoreRef

instance HFunctor MulticoreCMD
  where
    hfmap _ (OnCore coreId comp) = OnCore coreId comp


type HostCMD = Imp.ControlCMD
           :+: Imp.ThreadCMD
           :+: CoreHaltCMD
           :+: CoreChanCMD
           :+: BulkArrCMD LArr
           :+: BulkArrCMD SArr
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
              -> Length
              -> AllocCMD (Param3 prog exp pred) (DLArr a)
    AllocSArr :: pred a
              => Length
              -> AllocCMD (Param3 prog exp pred) (DSArr a)
    OnHost :: Host a -> AllocCMD (Param3 prog exp pred) a

instance HFunctor AllocCMD
  where
    hfmap _ (AllocLArr coreId size) = AllocLArr coreId size
    hfmap _ (AllocSArr size)        = AllocSArr size
    hfmap _ (OnHost host)           = OnHost host


newtype Multicore a = Multicore
    { unMulticore :: Program (AllocCMD :+: CoreChanAllocCMD) (Param2 Prim PrimType) a }
  deriving (Functor, Applicative, Monad)


runAllocCMD :: AllocCMD (Param3 Run Prim PrimType) a -> Run a
runAllocCMD (AllocLArr _ size) = LArr <$> newArr (value size)
runAllocCMD (AllocSArr   size) = SArr <$> newArr (value size)
runAllocCMD (OnHost host)      = runHost host

instance Interp AllocCMD Run (Param2 Prim PrimType)
  where interp = runAllocCMD

instance MonadRun Multicore where liftRun = interpret . unMulticore
