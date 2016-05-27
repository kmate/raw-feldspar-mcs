module Zeldspar.Multicore.Representation where

import Feldspar.Multicore.CoreId
import Feldspar.Multicore.Channel.Frontend
import Feldspar.Multicore.Representation
import Feldspar.Storable

import Ziria


type CoreZ inp out a = Z inp out CoreComp a

data MulticoreZ inp out a where
  OnCore  :: (CoreTransferable inp, CoreTransferable out)
          => CoreZ inp out a
          -> CoreId
          -> MulticoreZ inp out a
  Connect :: CoreTransferable mid
          => SizeSpec mid
          -> MulticoreZ inp mid b
          -> MulticoreZ mid out c
          -> MulticoreZ inp out ()
