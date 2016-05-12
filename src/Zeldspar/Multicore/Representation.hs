module Zeldspar.Multicore.Representation where

import Feldspar.Multicore.Channel
import Feldspar.Multicore.Representation

import Ziria


type CoreZ inp out = Z inp out CoreComp ()

data MulticoreZ inp out
  = OnCore (CoreZ inp out) CoreId
  | forall mid. Transferable mid
  => Connect (SizeSpec mid) (MulticoreZ inp mid) (MulticoreZ mid out)
