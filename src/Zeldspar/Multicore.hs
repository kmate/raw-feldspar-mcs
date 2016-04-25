module Zeldspar.Multicore
  ( module F
  , module Z
  ) where

import Feldspar.Multicore as F hiding (lift, forever)
import Zeldspar as Z
import Zeldspar.Parallel as Z hiding (translatePar)
import Zeldspar.Multicore.Compile as Z

