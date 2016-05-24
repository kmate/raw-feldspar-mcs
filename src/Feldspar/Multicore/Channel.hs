{-# LANGUAGE UndecidableInstances, FunctionalDependencies #-}
module Feldspar.Multicore.Channel where

import Control.Monad.Trans
import Data.Proxy

import Feldspar hiding ((==))
import Feldspar.Data.Vector hiding (ofLength, VecChanSizeSpec)
import Feldspar.Multicore.Frontend
import Feldspar.Multicore.Pipe
import Feldspar.Multicore.Reference
import Feldspar.Multicore.Representation


--------------------------------------------------------------------------------
-- Channel interface
--------------------------------------------------------------------------------

host :: CoreId
host = sharedId

data Chan a = Chan
            { cIsOpen      :: LocalRef Bool
            , cLastReadOK  :: LocalRef Bool
            , cPipeAdapter :: PipeAdapter a
            , cSizeSpec    :: SizeSpec a
            }

newChan :: ChanType a => CoreId -> CoreId -> SizeSpec a -> Multicore (Chan a)
newChan from to s = do
    isOpen     <- allocRef store
    lastReadOK <- allocRef store
    onHost $ do setLocalRef isOpen     true
                setLocalRef lastReadOK true
    pipeAdapter <- newPipeAdapter from to s
    return $ Chan isOpen lastReadOK pipeAdapter s
      where
        store = if host == from then to else from

readChan :: (MonadComp m, LocalRefAccess m, Transferable' m a)
         => Chan a -> Slot a -> m (Data Bool)
readChan (Chan isOpen lastReadOK pipeAdapter _) slot = do
    open <- getLocalRef isOpen
    setLocalRef lastReadOK open
    iff open (readPipeAdapter pipeAdapter (getLocalRef isOpen) slot) (return ())
    return open

writeChan :: (MonadComp m, LocalRefAccess m, Transferable' m a)
          => Chan a -> a -> m (Data Bool)
writeChan (Chan isOpen _ pipeAdapter _) v = do
    open <- getLocalRef isOpen
    ifE (open)
        (writePipeAdapter pipeAdapter (getLocalRef isOpen) v >> return true)
        (return false)

lastChanReadOK :: LocalRefAccess m => Chan a -> m (Data Bool)
lastChanReadOK (Chan _ lastReadOK _ _) = getLocalRef lastReadOK

closeChan :: LocalRefAccess m => Chan a -> m ()
closeChan (Chan isOpen _ _ _) = setLocalRef isOpen false


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
-- Pipe adapters and transferable classes
--------------------------------------------------------------------------------

data PipeAdapter a
  where
    HostToCore :: ChanType a => HostToCorePipe (ChanElemType a) -> PipeAdapter a
    CoreToHost :: ChanType a => CoreToHostPipe (ChanElemType a) -> PipeAdapter a
    CoreToCore :: ChanType a => CorePipe (ChanElemType a) -> PipeAdapter a

class PrimType (ChanElemType a) => ChanType a
  where
    type ChanElemType a :: *
    data SizeSpec a :: *

    newPipeAdapter :: CoreId -> CoreId -> SizeSpec a -> Multicore (PipeAdapter a)
    newPipeAdapter from to s
        | host == from = do
            p :: HostToCorePipe (ChanElemType a) <- allocHostPipe to l
            onHost $ initPipe p
            return $ HostToCore p
        | host == to   = do
            p :: CoreToHostPipe (ChanElemType a) <- allocHostPipe from l
            onHost $ initPipe p
            return $ CoreToHost p
        | otherwise = do
            p <- allocCorePipe from to l
            onHost $ initPipe p
            return $ CoreToCore p
        where
            l = pipeSize (Proxy :: Proxy a) s

    pipeSize :: proxy a -> SizeSpec a -> Length


class ChanType a => Transferable' m a
  where
    type Slot a :: *
    newSlot :: Chan a -> m (Slot a)
    getSlot :: Slot a -> m a

    readPipeAdapter  :: PipeAdapter a -> m (Data Bool) -> Slot a -> m ()
    writePipeAdapter :: PipeAdapter a -> m (Data Bool) -> a -> m ()

class    (Transferable' Host a, Transferable' CoreComp a) => Transferable a
instance (Transferable' Host a, Transferable' CoreComp a) => Transferable a

class TransferType m a t | a -> t
  where
    toTransfer   :: a -> m t
    fromTransfer :: t -> m a


--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance PrimType a => ChanType (Data a)
  where
    type ChanElemType (Data a) = a
    data SizeSpec     (Data a) = PrimChanSizeSpec Length
    pipeSize _ (PrimChanSizeSpec l) = l

instance PrimType a => Transferable' Host (Data a)
  where
    type Slot (Data a) = Ref a
    newSlot _ = newRef
    getSlot = getRef

    readPipeAdapter (CoreToHost p) cont s = do
        arr <- newArr 1
        pullPipeI cont p (0,0) arr
        elem :: Data a <- getArr 0 arr
        setRef s elem
    writePipeAdapter (HostToCore p) cont v = do
        arr <- newArr 1
        setArr 0 v arr
        pushPipeI cont p (0,0) arr

instance PrimType a => Transferable' CoreComp (Data a)
  where
    type Slot (Data a) = Ref a
    newSlot _ = newRef
    getSlot = getRef

    readPipeAdapter  (HostToCore p) cont s = readPipeI cont p >>= setRef s
    readPipeAdapter  (CoreToCore p) cont s = readPipeI cont p >>= setRef s
    writePipeAdapter (CoreToHost p) cont v = writePipeI cont v p
    writePipeAdapter (CoreToCore p) cont v = writePipeI cont v p

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
    type Slot (Store (DPull a)) = Store (DPull a)
    newSlot (Chan _ _ _ (VecChanSizeSpec _ l)) = newStore $ value l
    getSlot = return

    readPipeAdapter  (CoreToHost p) = readPipeAdapter'  p
    writePipeAdapter (HostToCore p) = writePipeAdapter' p

instance PrimType a => Transferable' CoreComp (Store (DPull a))
  where
    type Slot (Store (DPull a)) = Store (DPull a)
    newSlot (Chan _ _ _ (VecChanSizeSpec _ l)) = newStore $ value l
    getSlot = return

    readPipeAdapter  (HostToCore p) = readPipeAdapter'  p
    readPipeAdapter  (CoreToCore p) = readPipeAdapter'  p
    writePipeAdapter (CoreToHost p) = writePipeAdapter' p
    writePipeAdapter (CoreToCore p) = writePipeAdapter' p

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

readPipeAdapter' :: (MonadComp m, Wait m, Pipe p, BulkPipeReader p m, PrimType a)
                 => p a -> m (Data Bool) -> Slot (Store (DPull a)) -> m ()
readPipeAdapter' p cont s@(Store (lenRef, arr)) = do
    l <- unsafeFreezeRef lenRef
    pullPipeI cont p (0, l - 1) arr

writePipeAdapter' :: (MonadComp m, Wait m, Pipe p, BulkPipeWriter p m, PrimType a)
                  => p a -> m (Data Bool) -> Store (DPull a) -> m ()
writePipeAdapter' p cont (Store (lenRef, arr)) = do
    l <- unsafeFreezeRef lenRef
    pushPipeI cont p (0, l - 1) arr
