module Feldspar.Multicore
  ( lift
  , module Feldspar
  , module Feldspar.Run
  , module Feldspar.Multicore.Compile.Parallella
  , module Feldspar.Multicore.Frontend
  , module Feldspar.Multicore.Representation
  ) where

import Control.Monad.Trans (lift)

import Feldspar
import Feldspar.Run

import Feldspar.Multicore.Compile.Parallella
import Feldspar.Multicore.Frontend
import Feldspar.Multicore.Representation (AllocHost, Host)
