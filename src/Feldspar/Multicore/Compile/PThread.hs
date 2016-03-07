module Feldspar.Multicore.Compile.PThread where

import Control.Monad.Operational.Higher
import Data.Proxy

import Feldspar.Multicore.Compile.Platform
import Feldspar.Multicore.Representation

import Language.C.Monad


data PThread

pthread :: Platform PThread
pthread = Proxy


compAllocHostCMD :: AllocHostCMD (CGenFor PThread) a -> CGen a
compAllocHostCMD = error "TODO: compile for Pthread"

instance CompFor AllocHostCMD PThread where comp = compAllocHostCMD
