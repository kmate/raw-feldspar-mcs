module Feldspar.Multicore.Compile
  ( runIO, compile, icompile
  , module Feldspar.Multicore.Compile.Parallella
  , module Feldspar.Multicore.Compile.Platform
  , module Feldspar.Multicore.Compile.PThread
  ) where

import Control.Monad.Operational.Higher
import Text.PrettyPrint.Mainland (pretty)

import Feldspar.Multicore.Compile.Parallella
import Feldspar.Multicore.Compile.Platform
import Feldspar.Multicore.Compile.PThread
import Feldspar.Multicore.Representation
import Language.C.Monad


runIO :: AllocHost a -> IO a
runIO = interpret


compile :: CompFor AllocHostCMD p => Platform p -> AllocHost a -> String
compile p = pretty 80 . prettyCGen . runCGenFor p . interpret

icompile :: CompFor AllocHostCMD p => Platform p -> AllocHost a -> IO ()
icompile p = putStrLn . compile p
