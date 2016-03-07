module Feldspar.Multicore.Compile.Platform
  ( Platform
  , CGenFor, runCGenFor
  ) where

import Data.Proxy

import Language.C.Monad


type Platform a = Proxy a

newtype CGenFor platform a = CGenFor { unCGenFor :: CGen a }
  deriving (Functor, Applicative, Monad)

runCGenFor :: Platform p -> CGenFor p a -> CGen a
runCGenFor _ = unCGenFor
