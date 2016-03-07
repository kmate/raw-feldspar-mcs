module Feldspar.Multicore.Compile.Platform
  ( Platform
  , CGenFor, runCGenFor
  , CompFor (comp)
  ) where

import Control.Monad.Operational.Higher
import Data.Proxy

import Language.C.Monad


type Platform a = Proxy a

newtype CGenFor platform a = CGenFor { unCGenFor :: CGen a }
  deriving (Functor, Applicative, Monad)

runCGenFor :: Platform p -> CGenFor p a -> CGen a
runCGenFor _ = unCGenFor


class CompFor i p
  where
    comp :: i (CGenFor p) a -> CGen a

instance CompFor i p => Interp i (CGenFor p)
  where
    interp = CGenFor . comp
