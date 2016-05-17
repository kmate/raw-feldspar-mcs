module Zeldspar.Multicore.Representation where

import Feldspar.Multicore.Channel
import Feldspar.Multicore.Representation

import Ziria


type CoreZ inp out a = Z inp out CoreComp a

data MulticoreZ inp out a where
  OnCore  :: CoreZ inp out a -> CoreId -> MulticoreZ inp out a
  Connect :: Transferable mid
          => SizeSpec mid
          -> MulticoreZ inp mid b
          -> MulticoreZ mid out c
          -> MulticoreZ inp out ()
