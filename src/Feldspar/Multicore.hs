module Feldspar.Multicore
  ( lift, module F
  ) where

import Control.Monad.Trans (lift)

import Feldspar as F
import Feldspar.Run as F

import Feldspar.Multicore.Compile.Parallella as F
import Feldspar.Multicore.Frontend as F
import Feldspar.Multicore.Representation as F (CoreId, Size, IndexRange, LocalArr, CoreComp, Multicore, Host)
