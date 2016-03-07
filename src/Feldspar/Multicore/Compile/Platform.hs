{-# LANGUAGE FunctionalDependencies #-}
module Feldspar.Multicore.Compile.Platform
  ( Platform
  , CGenTFor, runCGenTFor
  , CompFor (comp, unwrap)
  ) where

import Control.Monad.Operational.Higher
import Data.Proxy

import Language.C.Monad


type Platform a = Proxy a

newtype CGenTFor platform m a = CGenTFor { unCGenTFor :: CGenT m a }
  deriving (Functor, Applicative, Monad)

runCGenTFor :: Platform p -> CGenTFor p m a -> CGenT m a
runCGenTFor _ = unCGenTFor


class CompFor i p m | i p -> m
  where
    comp :: i (CGenTFor p m) a -> CGenT m a
    unwrap :: Platform p -> Proxy i -> m a -> a

instance CompFor i p m => Interp i (CGenTFor p m)
  where
    interp = CGenTFor . comp
