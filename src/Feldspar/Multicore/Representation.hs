module Feldspar.Multicore.Representation where

import Data.ALaCarte
import Data.Word

import Control.Monad.Operational.Higher

import Feldspar
import Feldspar.Representation
import Feldspar.Run.Representation


type CoreId = Word32
type Size   = Word32

data LocalArr a
    = LocalArr

data AllocCMD (prog :: * -> *) a
  where
    Alloc :: CoreId -> Size -> AllocCMD prog (LocalArr a)

data MulticoreCMD (prog :: * -> *) a
  where
    Fetch  :: LocalArr a -> (Data Index, Data Index) -> Arr a -> MulticoreCMD prog ()
    Flush  :: LocalArr a -> (Data Index, Data Index) -> Arr a -> MulticoreCMD prog ()
    OnCore :: CoreId -> Comp () -> MulticoreCMD prog ()

type HostCMD
    =   RunCMD
    :+: MulticoreCMD

newtype Host a = Host { unHost :: ProgramT HostCMD (Program CompCMD) a }
    deriving (Functor, Applicative, Monad)
