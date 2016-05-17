module Zeldspar.Multicore.Representation where

import Feldspar.Multicore.Channel
import Feldspar.Multicore.Representation
import Feldspar.Storable

import Ziria


type CoreZ inp out a = Z inp out CoreComp a

data MulticoreZ inp out a where
  OnCore  :: CoreZ inp out a -> CoreId -> MulticoreZ inp out a
  Connect :: Transferable mid
          => SizeSpec mid
          -> MulticoreZ inp mid b
          -> MulticoreZ mid out c
          -> MulticoreZ inp out ()

instance Transferable a => ZType a CoreComp
  where
    type ZInternal a CoreComp = TransferType a
    wrap   = fromTransfer
    unwrap = toTransfer

-- Vectors are transfered as their stores, and that is needed to be storable.
instance Storable a => Storable (Store a)
  where
    type StoreRep (Store a)  = StoreRep a
    type StoreSize (Store a) = StoreSize a
    newStoreRep = error "TODO: forward"
    initStoreRep = error "TODO: forward"
    readStoreRep =  error "TODO: forward"
    unsafeFreezeStoreRep = error "TODO: forward"
    writeStoreRep = error "TODO: forward"
    copyStoreRep = error "TODO: forward"
