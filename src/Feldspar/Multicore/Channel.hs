{-# LANGUAGE UndecidableInstances, FunctionalDependencies #-}
module Feldspar.Multicore.Channel where

import Data.Proxy

import Feldspar hiding ((==))
import Feldspar.Data.Vector hiding (ofLength, VecChanSizeSpec)
import Feldspar.Multicore.Frontend
import Feldspar.Multicore.Pipe
import Feldspar.Multicore.Representation


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
    data SizeSpec a :: *

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
    readChan  :: Chan a -> m a
    writeChan :: Chan a -> a -> m ()

class    (Transferable' Host a, Transferable' CoreComp a) => Transferable a
instance (Transferable' Host a, Transferable' CoreComp a) => Transferable a

class TransferType m a t | a -> t
  where
    toTransfer   :: a -> m t
    fromTransfer :: t -> m a


ofLength :: Length -> Length -> SizeSpec (Store (DPull a))
ofLength = VecChanSizeSpec

instance Num (SizeSpec (Data a))
  where
    fromInteger n = PrimChanSizeSpec $ fromIntegral n
    (PrimChanSizeSpec n) + (PrimChanSizeSpec m) = PrimChanSizeSpec $ n + m
    (PrimChanSizeSpec n) - (PrimChanSizeSpec m) = PrimChanSizeSpec $ n - m
    (PrimChanSizeSpec n) * (PrimChanSizeSpec m) = PrimChanSizeSpec $ n * m
    abs (PrimChanSizeSpec n) = PrimChanSizeSpec $ abs n
    signum (PrimChanSizeSpec n) = PrimChanSizeSpec $ signum n

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance PrimType a => ChanType (Data a)
  where
    type ChanElemType (Data a) = a
    data SizeSpec (Data a)     = PrimChanSizeSpec Length
    pipeSize _ (PrimChanSizeSpec l) = l

instance PrimType a => Transferable' Host (Data a)
  where
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
    readChan (HostToCoreChan _ p) = readPipe p
    readChan (CoreToCoreChan _ p) = readPipe p
    writeChan (CoreToHostChan _ p) v = writePipe v p
    writeChan (CoreToCoreChan _ p) v = writePipe v p

instance (Monad m, PrimType a) => TransferType m (Data a) (Data a)
  where
    toTransfer   = return
    fromTransfer = return


instance PrimType a => ChanType (Store (DPull a))
  where
    type ChanElemType (Store (DPull a)) = a
    data SizeSpec     (Store (DPull a)) = VecChanSizeSpec Length Length
    pipeSize _ (VecChanSizeSpec n m) = n * m

instance PrimType a => Transferable' Host (Store (DPull a))
  where
    readChan  (CoreToHostChan s p) = readChan'  s p
    writeChan (HostToCoreChan s p) = writeChan' s p

instance PrimType a => Transferable' CoreComp (Store (DPull a))
  where
    readChan  (HostToCoreChan s p) = readChan'  s p
    readChan  (CoreToCoreChan s p) = readChan'  s p
    writeChan (CoreToHostChan s p) = writeChan' s p
    writeChan (CoreToCoreChan s p) = writeChan' s p

instance (Monad m, PrimType a) => TransferType m (Store (DPull a)) (Store (DPull a))
  where
    toTransfer   = return
    fromTransfer = return

instance (MonadComp m, PrimType a) => TransferType m (DPull a) (Store (DPull a))
  where
    toTransfer   = initStore
    fromTransfer = unsafeFreezeStore

instance (MonadComp m, PrimType a) => TransferType m (DPush a) (Store (DPull a))
  where
    toTransfer (Push len dump) = do
        s@(Store (_, arr)) <- newStore len
        let write i v = setArr i v arr
        dump write
        return s
    fromTransfer (Store (lenRef, arr)) = do
        len <- getRef lenRef
        return $ Push len $ \write ->
            for (0, 1, Excl len) $ \i -> do
              v <- getArr i arr
              write i v

instance (MonadComp m, PrimType a) => TransferType m (Dim1 (Arr a)) (Store (DPull a))
  where
    toTransfer x = do
        lenRef <- initRef $ dimLength x
        return $ Store (lenRef, dim1_inner x)
    fromTransfer (Store (lenRef, arr)) = do
        len <- getRef lenRef
        return $ Dim1 len arr

readChan' :: (MonadComp m, Wait m, Pipe p, BulkPipeReader p m, PrimType a)
          => SizeSpec (Store (DPull a)) -> p a -> m (Store (DPull a))
readChan' (VecChanSizeSpec _ l) p = do
    let l' :: Data Length = value l
    arr <- newArr l'
    pullPipe p (0, l' - 1) arr
    lenRef :: Ref Length <- initRef l'
    return $ Store (lenRef, arr)

writeChan' :: (MonadComp m, Wait m, Pipe p, BulkPipeWriter p m, PrimType a)
           => SizeSpec (Store (DPull a)) -> p a -> Store (DPull a) -> m ()
writeChan' (VecChanSizeSpec _ l) p (Store (_, arr)) = do
    let l' :: Data Length = value l
    pushPipe p (0, l' - 1) arr
