module Feldspar.Multicore.Compile
  ( runIO, compile, icompile
  , module Feldspar.Multicore.Compile.Parallella
  , module Feldspar.Multicore.Compile.Platform
  , module Feldspar.Multicore.Compile.PThread
  ) where

import Control.Monad.Fix
import Control.Monad.Operational.Higher
import Data.Proxy
import Text.PrettyPrint.Mainland (pretty)

import Feldspar.Multicore.Compile.Parallella
import Feldspar.Multicore.Compile.Platform
import Feldspar.Multicore.Compile.PThread
import Feldspar.Multicore.Representation

import Language.C.Monad
import Language.Embedded.CExp


runIO :: AllocHost a -> IO a
runIO = interpret


compile :: (MonadFix m, CompFor (AllocHostCMD CExp) p m) => Platform p -> AllocHost a -> String
compile p
    = pretty 80
    . unwrap p (Proxy :: Proxy (AllocHostCMD CExp))
    . prettyCGenT . wrapMain . runCGenTFor p
    . interpret

icompile :: (MonadFix m, CompFor (AllocHostCMD CExp) p m) => Platform p -> AllocHost a -> IO ()
icompile p = putStrLn . compile p
