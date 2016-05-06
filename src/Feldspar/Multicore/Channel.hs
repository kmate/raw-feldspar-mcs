{-# LANGUAGE UndecidableInstances #-}
module Feldspar.Multicore.Channel where

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

class ChanType a
  where
    type ChanElemType a :: *
    type SizeSpec a :: *
    newChan :: CoreId -> CoreId -> SizeSpec a -> Multicore (Chan a)

class ChanType a => Transferable' m a
  where
    readChan :: Chan a -> m a
    writeChan :: Chan a -> a -> m ()

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
    newChan from to l
        | host == from = do
            p :: HostToCorePipe a <- allocHostPipe to l
            onHost $ initPipe p
            return $ HostToCoreChan l p
        | host == to   = do
            p :: CoreToHostPipe a <- allocHostPipe from l
            onHost $ initPipe p
            return $ CoreToHostChan l p
        | otherwise = do
            p <- allocCorePipe from to l
            onHost $ initPipe p
            return $ CoreToCoreChan l p

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


instance PrimType a => ChanType (Vector (Data a))
  where
    type ChanElemType (Vector (Data a)) = a
    type SizeSpec     (Vector (Data a)) = VecChanSizeSpec
    newChan from to s@(VecChanSizeSpec n m)
        | host == from = do
            p :: HostToCorePipe a <- allocHostPipe to l
            onHost $ initPipe p
            return $ HostToCoreChan s p
        | host == to   = do
            p :: CoreToHostPipe a <- allocHostPipe from l
            onHost $ initPipe p
            return $ CoreToHostChan s p
        | otherwise = do
            p <- allocCorePipe from to l
            onHost $ initPipe p
            return $ CoreToCoreChan s p
      where
        l = n * m


instance PrimType a => Transferable' Host (Vector (Data a))
  where
    readChan  (CoreToHostChan s p) = readChan'  s p
    writeChan (HostToCoreChan s p) = writeChan' s p

instance PrimType a => Transferable' CoreComp (Vector (Data a))
  where
    readChan  (HostToCoreChan s p) = readChan'  s p
    readChan  (CoreToCoreChan s p) = readChan'  s p
    writeChan (CoreToHostChan s p) = writeChan' s p
    writeChan (CoreToCoreChan s p) = writeChan' s p

readChan' :: (MonadComp m, Wait m, Pipe p, BulkPipeReader p m, PrimType a)
          => VecChanSizeSpec -> p a -> m (Vector (Data a))
readChan' (VecChanSizeSpec _ l) p = do
    let l' :: Data Length = value l
    arr <- newArr l'
    pullPipe p (0, l' - 1) arr
    lenRef :: Ref Length <- initRef l'
    unsafeFreezeStore $ Store (lenRef, arr)

writeChan' :: (MonadComp m, Wait m, Pipe p, BulkPipeWriter p m, PrimType a)
           => VecChanSizeSpec -> p a -> Vector (Data a) -> m ()
writeChan' (VecChanSizeSpec _ l) p v = do
    let l' :: Data Length = value l
    Store (_, arr) <- initStore v
    pushPipe p (0, l' - 1) arr
