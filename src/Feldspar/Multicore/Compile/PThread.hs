module Feldspar.Multicore.Compile.PThread where

import Control.Monad.Operational.Higher
import Data.Proxy

import Feldspar.Multicore.Compile.Platform
import Feldspar.Multicore.Representation


data PThread

pthread :: Platform PThread
pthread = Proxy

instance Interp AllocHostCMD (CGenFor PThread) where interp = error "TODO: compile for Pthread"
