{-# LANGUAGE UndecidableInstances, FunctionalDependencies #-}
module Feldspar.Multicore.Channel.Frontend where

import Control.Monad.Operational.Higher
import Control.Monad.Trans
import Data.Ix
import Data.Proxy
import Data.Typeable
import Data.TypedStruct

import Feldspar hiding ((==))
import Feldspar.Data.Vector hiding (ofLength, VecChanSizeSpec)
import Feldspar.Multicore.CoreId
import qualified Feldspar.Multicore.Channel.Representation as Rep
import Feldspar.Multicore.Frontend
import Feldspar.Multicore.Representation hiding (CoreId)
import Feldspar.Representation

import qualified Language.Embedded.Concurrent.CMD as Imp
import qualified Language.Embedded.Expression as Imp
import qualified Language.Embedded.Imperative as Imp


--------------------------------------------------------------------------------
-- Channel interface
--------------------------------------------------------------------------------

data CoreChan a
  where
    CoreChan :: CoreChanType a
             => SizeSpec a
             -> Rep.CoreChan (ElemType a)
             -> CoreChan a

class PrimType (ElemType a) => CoreChanType a
  where
    type ElemType a :: *
    data SizeSpec a :: *
    newChan :: CoreId -> CoreId -> SizeSpec a -> Multicore (CoreChan a)

class CoreChanType a => CoreTransferable' m a
  where
    type Slot a :: *
    newSlot :: CoreChan a -> m (Slot a)
    getSlot :: Slot a -> m a

    readChan  :: CoreChan a -> Slot a -> m (Data Bool)
    writeChan :: CoreChan a -> a -> m (Data Bool)
    closeChan :: CoreChan a -> m ()

class CoreTransferType m a t | a -> t
  where
    toTransfer   :: a -> m t
    fromTransfer :: t -> m a

class    (CoreTransferable' Host a, CoreTransferable' CoreComp a) => CoreTransferable a
instance (CoreTransferable' Host a, CoreTransferable' CoreComp a) => CoreTransferable a


one :: PrimType a => SizeSpec (Data a)
one = SingleItem


instance Num (SizeSpec (Store (DPull a)))
  where
    fromInteger n = VecChanSizeSpec $ fromIntegral n
    (VecChanSizeSpec n) + (VecChanSizeSpec m) = VecChanSizeSpec $ n + m
    (VecChanSizeSpec n) - (VecChanSizeSpec m) = VecChanSizeSpec $ n - m
    (VecChanSizeSpec n) * (VecChanSizeSpec m) = VecChanSizeSpec $ n * m
    abs (VecChanSizeSpec n) = VecChanSizeSpec $ abs n
    signum (VecChanSizeSpec n) = VecChanSizeSpec $ signum n


--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance PrimType a => CoreChanType (Data a)
  where
    type ElemType (Data a) = a
    data SizeSpec (Data a) = SingleItem
    newChan f t sz
      = Multicore $ fmap (CoreChan sz) $ singleInj
      $ Rep.NewChan f t 1

instance PrimType a => CoreTransferable' Host (Data a)
  where
    type Slot (Data a) = Arr a
    newSlot _ = newArr 1
    getSlot = getArr 0

    readChan (CoreChan _ c) (Arr _ (Single (arr)))
      = Host $ readChanBuf' c 0 1 arr
    writeChan (CoreChan _ c) x = Host $ writeChan' c x
    closeChan (CoreChan _ c) = Host $ closeChan' c

instance PrimType a => CoreTransferable' CoreComp (Data a)
  where
    type Slot (Data a) = Arr a
    newSlot _ = newArr 1
    getSlot = getArr 0

    readChan (CoreChan _ c) (Arr _ (Single (arr)))
      = CoreComp $ readChanBuf' c 0 1 arr
    writeChan (CoreChan _ c) x = CoreComp $ writeChan' c x
    closeChan (CoreChan _ c) = CoreComp $ closeChan' c

instance (Monad m, PrimType a) => CoreTransferType m (Data a) (Data a)
  where
    toTransfer   = return
    fromTransfer = return


instance PrimType a => CoreChanType (Store (DPull a))
  where
    type ElemType (Store (DPull a)) = a
    data SizeSpec (Store (DPull a)) = VecChanSizeSpec Length
    newChan f t sz@(VecChanSizeSpec l)
      = Multicore $ fmap (CoreChan sz) $ singleInj
      $ Rep.NewChan f t l

instance PrimType a => CoreTransferable' Host (Store (DPull a))
  where
    type Slot (Store (DPull a)) = Store (DPull a)
    newSlot (CoreChan (VecChanSizeSpec l) _) = newStore $ value l
    getSlot = return

    readChan (CoreChan _ c) s@(Store (lenRef, (Arr _ (Single (arr))))) = do
        l :: Data Length <- unsafeFreezeRef lenRef
        Host $ readChanBuf' c 0 l arr
    writeChan (CoreChan _ c) s@(Store (lenRef, (Arr _ (Single (arr))))) = do
        l :: Data Length <- unsafeFreezeRef lenRef
        Host $ writeChanBuf' c 0 l arr
    closeChan (CoreChan _ c) = Host $ closeChan' c

instance PrimType a => CoreTransferable' CoreComp (Store (DPull a))
  where
    type Slot (Store (DPull a)) = Store (DPull a)
    newSlot (CoreChan (VecChanSizeSpec l) _) = newStore $ value l
    getSlot = return

    readChan (CoreChan _ c) s@(Store (lenRef, (Arr _ (Single (arr))))) = do
        l :: Data Length <- unsafeFreezeRef lenRef
        CoreComp $ readChanBuf' c 0 l arr
    writeChan (CoreChan _ c) s@(Store (lenRef, (Arr _ (Single (arr))))) = do
        l :: Data Length <- unsafeFreezeRef lenRef
        CoreComp $ writeChanBuf' c 0 l arr
    closeChan (CoreChan _ c) = CoreComp $ closeChan' c

instance (Monad m, PrimType a) => CoreTransferType m (Store (DPull a)) (Store (DPull a))
  where
    toTransfer   = return
    fromTransfer = return

instance (MonadComp m, PrimType a) => CoreTransferType m (DPull a) (Store (DPull a))
  where
    toTransfer   = initStore
    fromTransfer = unsafeFreezeStore

instance (MonadComp m, PrimType a) => CoreTransferType m (DPush a) (Store (DPull a))
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

instance (MonadComp m, PrimType a) => CoreTransferType m (Dim1 (Arr a)) (Store (DPull a))
  where
    toTransfer x = do
        lenRef <- initRef $ dimLength x
        return $ Store (lenRef, dim1_inner x)
    fromTransfer (Store (lenRef, arr)) = do
        len <- getRef lenRef
        return $ Dim1 len arr


--------------------------------------------------------------------------------
-- Representation wrappers
--------------------------------------------------------------------------------

readChanBuf' :: ( Typeable a, pred a
                , Ix i, Integral i
                , Imp.FreeExp exp, Imp.FreePred exp Bool
                , Rep.CoreChanCMD :<: instr, Monad m )
              => Rep.CoreChan a
             -> exp i -- ^ Offset in array to start writing
             -> exp i -- ^ Elements to read
             -> Imp.Arr i a
             -> ProgramT instr (Param2 exp pred) m (exp Bool)
readChanBuf' ch off sz arr = fmap Imp.valToExp . singleInj $ Rep.ReadChan ch off sz arr

writeChan' :: ( Typeable a, pred a
              , Imp.FreeExp exp, Imp.FreePred exp Bool
              , Rep.CoreChanCMD :<: instr, Monad m )
           => Rep.CoreChan a
           -> exp a
           -> ProgramT instr (Param2 exp pred) m (exp Bool)
writeChan' c = fmap Imp.valToExp . singleInj . Rep.WriteOne c

writeChanBuf' :: ( Typeable a, pred a
                 , Ix i, Integral i
                 , Imp.FreeExp exp, Imp.FreePred exp Bool
                 , Rep.CoreChanCMD :<: instr, Monad m )
              => Rep.CoreChan a
              -> exp i -- ^ Offset in array to start reading
              -> exp i -- ^ Elements to write
              -> Imp.Arr i a
              -> ProgramT instr (Param2 exp pred) m (exp Bool)
writeChanBuf' ch off sz arr = fmap Imp.valToExp . singleInj $ Rep.WriteChan ch off sz arr

closeChan' :: (Rep.CoreChanCMD :<: instr)
           => Rep.CoreChan a
           -> ProgramT instr (Param2 exp pred) m ()
closeChan' = singleInj . Rep.CloseChan
