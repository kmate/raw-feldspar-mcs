module Feldspar.Multicore
  ( lift
  , module Feldspar
  , module Feldspar.Run
  , module Feldspar.Multicore.Compile
  , module Feldspar.Multicore.Frontend
  , module Feldspar.Multicore.Representation
  ) where

import Control.Monad.Trans (lift)

import Feldspar
import Feldspar.Run hiding (runIO, compile, icompile)

import Feldspar.Multicore.Compile
import Feldspar.Multicore.Frontend
import Feldspar.Multicore.Representation (AllocHost, HostT)
