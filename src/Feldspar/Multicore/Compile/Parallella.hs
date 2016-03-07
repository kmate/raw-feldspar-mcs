module Feldspar.Multicore.Compile.Parallella where

import Control.Monad.Operational.Higher
import Data.Proxy

import Feldspar.Multicore.Compile.Platform
import Feldspar.Multicore.Representation


data Parallella

parallella :: Platform Parallella
parallella = Proxy

instance CompFor AllocHostCMD Parallella where comp = error "TODO: compile for Parallella"
