module Zeldspar.Multicore
  ( module F
  , module Z
  , liftHost
  ) where

import Control.Monad.Trans as C (lift)
import Feldspar.Multicore as F hiding (lift, forever, ofLength, VecChanSizeSpec)
import Zeldspar as Z
import Zeldspar.Multicore.Compile as Z
import Zeldspar.Multicore.Representation as Z

liftHost :: Run a -> Host a
liftHost = C.lift
