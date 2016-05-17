{-# LANGUAGE UndecidableInstances #-}
module Feldspar.Multicore.Channel where

import Data.Proxy

import Feldspar hiding ((==))
import Feldspar.Multicore.Frontend
import Feldspar.Multicore.Pipe
import Feldspar.Multicore.Representation
import Feldspar.Vector hiding (ofLength, VecChanSizeSpec)


--------------------------------------------------------------------------------
-- Channel interface
--------------------------------------------------------------------------------

host :: CoreId
host = sharedId

data Chan a
  where
    HostToCoreChan :: ChanType a
                   => SizeSpec a
                   -> HostToCorePipe (ChanElemType a)
                   -> Chan a

    CoreToHostChan :: ChanType a
                   => SizeSpec a
                   -> CoreToHostPipe (ChanElemType a)
                   -> Chan a

    CoreToCoreChan :: ChanType a
                   => SizeSpec a
                   -> CorePipe (ChanElemType a)
                   -> Chan a

class PrimType (ChanElemType a) => ChanType a
  where
    type ChanElemType a :: *
    type SizeSpec a :: *

    newChan :: CoreId -> CoreId -> SizeSpec a -> Multicore (Chan a)
    newChan from to s
        | host == from = do
            p :: HostToCorePipe (ChanElemType a) <- allocHostPipe to l
            onHost $ initPipe p
            return $ HostToCoreChan s p
        | host == to   = do
            p :: CoreToHostPipe (ChanElemType a) <- allocHostPipe from l
            onHost $ initPipe p
            return $ CoreToHostChan s p
        | otherwise = do
            p <- allocCorePipe from to l
            onHost $ initPipe p
            return $ CoreToCoreChan s p
        where
            l = pipeSize (Proxy :: Proxy a) s

    pipeSize :: proxy a -> SizeSpec a -> Length

class ChanType a => Transferable' m a
  where
    type TransferType a :: *
    toTransfer   :: a -> m (TransferType a)
    fromTransfer :: TransferType a -> m a
    readChan     :: Chan a -> m (TransferType a)
    writeChan    :: Chan a -> TransferType a -> m ()

class    (Transferable' Host a, Transferable' CoreComp a) => Transferable a
instance (Transferable' Host a, Transferable' CoreComp a) => Transferable a


data VecChanSizeSpec = VecChanSizeSpec Length Length

ofLength :: Length -> Length -> VecChanSizeSpec
ofLength = VecChanSizeSpec


--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance PrimType a => ChanType (Data a)
  where
    type ChanElemType (Data a) = a
    type SizeSpec (Data a)     = Length
    pipeSize _ = id


instance PrimType a => Transferable' Host (Data a)
  where
    type TransferType (Data a) = Data a
    toTransfer   = return
    fromTransfer = return
    readChan (CoreToHostChan _ p) = do
        arr <- newArr 1
        pullPipe p (0,0) arr
        elem <- getArr 0 arr
        return elem
    writeChan (HostToCoreChan _ p) v = do
        arr <- newArr 1
        setArr 0 v arr
        pushPipe p (0,0) arr

instance PrimType a => Transferable' CoreComp (Data a)
  where
    type TransferType (Data a) = Data a
    toTransfer   = return
    fromTransfer = return
    readChan (HostToCoreChan _ p) = readPipe p
    readChan (CoreToCoreChan _ p) = readPipe p
    writeChan (CoreToHostChan _ p) v = writePipe v p
    writeChan (CoreToCoreChan _ p) v = writePipe v p


instance PrimType a => ChanType (Vector (Data a))
  where
    type ChanElemType (Vector (Data a)) = a
    type SizeSpec     (Vector (Data a)) = VecChanSizeSpec
    pipeSize _ (VecChanSizeSpec n m) = n * m


instance PrimType a => Transferable' Host (Vector (Data a))
  where
    type TransferType (Vector (Data a)) = Store (Vector (Data a))
    toTransfer   = initStore
    fromTransfer = unsafeFreezeStore
    readChan  (CoreToHostChan s p) = readChan'  s p
    writeChan (HostToCoreChan s p) = writeChan' s p

instance PrimType a => Transferable' CoreComp (Vector (Data a))
  where
    type TransferType (Vector (Data a)) = Store (Vector (Data a))
    toTransfer   = initStore
    fromTransfer = unsafeFreezeStore
    readChan  (HostToCoreChan s p) = readChan'  s p
    readChan  (CoreToCoreChan s p) = readChan'  s p
    writeChan (CoreToHostChan s p) = writeChan' s p
    writeChan (CoreToCoreChan s p) = writeChan' s p

readChan' :: (MonadComp m, Wait m, Pipe p, BulkPipeReader p m, PrimType a)
          => VecChanSizeSpec -> p a -> m (Store (Vector (Data a)))
readChan' (VecChanSizeSpec _ l) p = do
    let l' :: Data Length = value l
    arr <- newArr l'
    pullPipe p (0, l' - 1) arr
    lenRef :: Ref Length <- initRef l'
    return $ Store (lenRef, arr)

writeChan' :: (MonadComp m, Wait m, Pipe p, BulkPipeWriter p m, PrimType a)
           => VecChanSizeSpec -> p a -> Store (Vector (Data a)) -> m ()
writeChan' (VecChanSizeSpec _ l) p (Store (_, arr)) = do
    let l' :: Data Length = value l
    pushPipe p (0, l' - 1) arr
