module Feldspar.Multicore.Representation where

import Control.Monad.Operational.Higher
import Control.Monad.Trans

import Data.ALaCarte
import Data.Word

import Feldspar
import Feldspar.Run.Frontend
import Feldspar.Run.Representation

import qualified Language.Embedded.Imperative as Imp
import qualified Language.Embedded.Imperative.CMD as Imp


type CoreId = Word32
type Size   = Word32
type Range i = (i, i)


--------------------------------------------------------------------------------
-- Host layer
--------------------------------------------------------------------------------

newtype Host a = Host { unHost :: Run a }
  deriving (Functor, Applicative, Monad)

instance MonadComp Host where
    liftComp        = Host . liftComp
    iff c t f       = Host $ Run $ Imp.iff c (unRun $ unHost t) (unRun $ unHost f)
    for  range body = Host $ Run $ Imp.for range (unRun . unHost . body)
    while cont body = Host $ Run $ Imp.while (unRun $ unHost cont) (unRun $ unHost body)

instance (a ~ ()) => PrintfType (Host a)
  where
    fprf h form = Host . Run . singleE . Imp.FPrintf h form . reverse


--------------------------------------------------------------------------------
-- Allocation layer
--------------------------------------------------------------------------------

data AllocHostCMD (prog :: * -> *) a
  where
    Alloc :: CoreId -> Size -> AllocHostCMD prog (Arr a)
    RunHost :: Host a -> AllocHostCMD prog a

instance HFunctor AllocHostCMD
  where
    hfmap _ (Alloc coreId size) = Alloc coreId size
    hfmap _ (RunHost host)      = RunHost host

type AllocHost a = Program AllocHostCMD a
