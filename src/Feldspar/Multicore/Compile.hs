module Feldspar.Multicore.Compile where

import Control.Monad.Operational.Higher

import Feldspar.Multicore.Representation


runIO :: AllocHost a -> IO a
runIO = interpret


-- TODO: implement

compile :: AllocHost a -> String
compile  = undefined

icompile :: AllocHost a -> IO ()
icompile  = putStrLn . compile
