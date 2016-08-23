module Feldspar.Multicore
  ( lift, module F
  ) where

import Control.Monad.Trans (lift)

import Feldspar as F hiding (forever)
import Feldspar.Data.Storable as F hiding (forever)
import Feldspar.Run as F hiding (forever)

import Feldspar.Multicore.CoreId as F
import Feldspar.Multicore.Channel.Frontend as F
import Feldspar.Multicore.Compile.Parallella as F
import Feldspar.Multicore.Frontend as F
import Feldspar.Multicore.Reference as F
import Feldspar.Multicore.Representation as F (IndexRange, LocalArr, SharedArr, CoreComp, Multicore, Host)
