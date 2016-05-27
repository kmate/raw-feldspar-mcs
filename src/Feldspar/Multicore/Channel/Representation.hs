{-# LANGUAGE UndecidableInstances #-}
module Feldspar.Multicore.Channel.Representation where

import Control.Monad.Operational.Higher
import Control.Monad.Trans
import Data.Ix
import Data.Proxy
import Data.Typeable
import GHC.Exts (Constraint)

import Feldspar
import Feldspar.Multicore.CoreId
import Feldspar.Primitive.Representation
import Feldspar.Representation
import Feldspar.Run
import Feldspar.Run.Concurrent
import Feldspar.Run.Representation

import qualified Language.Embedded.Concurrent as Imp
import qualified Language.Embedded.Concurrent.CMD as Imp
import qualified Language.Embedded.Expression as Imp
import qualified Language.Embedded.Imperative as Imp


data CoreChan (a :: *)
  = CoreChanRun (Imp.Chan Imp.Closeable a)
  | CoreChanComp


data CoreChanAllocCMD fs a where
  NewChan :: pred a
          => CoreId
          -> CoreId
          -> Length
          -> CoreChanAllocCMD (Param3 prog exp pred) (CoreChan a)


instance HFunctor CoreChanAllocCMD where
  hfmap _ (NewChan f t sz) = NewChan f t sz

instance HBifunctor CoreChanAllocCMD where
  hbimap _ _ (NewChan f t sz) = NewChan f t sz

instance (CoreChanAllocCMD :<: instr) => Reexpressible CoreChanAllocCMD instr where
  reexpressInstrEnv reexp (NewChan f t sz) = lift $ singleInj $ NewChan f t sz


runCoreChanAllocCMD :: CoreChanAllocCMD (Param3 Run Prim PrimType) a -> Run a
runCoreChanAllocCMD cmd@(NewChan _ _ sz) = do
    let size = (value sz) `Imp.timesSizeOf` (chanElemType cmd)
    c <- Run $ Imp.newCloseableChan' size
    return $ CoreChanRun c
      where
        chanElemType :: cmd (Param3 p e pred) (CoreChan t) -> Proxy t
        chanElemType _ = Proxy

instance Interp CoreChanAllocCMD Run (Param2 Prim PrimType)
  where interp = runCoreChanAllocCMD


data CoreChanCMD fs a where
  ReadOne   :: (Typeable a, pred a)
            => CoreChan a -> CoreChanCMD (Param3 prog exp pred) (Imp.Val a)
  WriteOne  :: (Typeable a, pred a)
            => CoreChan a -> exp a
            -> CoreChanCMD (Param3 prog exp pred) (Imp.Val Bool)

  ReadChan  :: (Typeable a, pred a, Ix i, Integral i)
            => CoreChan c -> exp i -> exp i
            -> Imp.Arr i a -> CoreChanCMD (Param3 prog exp pred) (Imp.Val Bool)
  WriteChan :: (Typeable a, pred a, Ix i, Integral i)
            => CoreChan c -> exp i -> exp i
            -> Imp.Arr i a -> CoreChanCMD (Param3 prog exp pred) (Imp.Val Bool)

  CloseChan :: CoreChan a -> CoreChanCMD (Param3 prog exp pred) ()
  ReadOK    :: CoreChan a -> CoreChanCMD (Param3 prog exp pred) (Imp.Val Bool)


instance HFunctor CoreChanCMD where
  hfmap _ (ReadOne c)         = ReadOne c
  hfmap _ (ReadChan c f t a)  = ReadChan c f t a
  hfmap _ (WriteOne c x)      = WriteOne c x
  hfmap _ (WriteChan c f t a) = WriteChan c f t a
  hfmap _ (CloseChan c)       = CloseChan c
  hfmap _ (ReadOK c)          = ReadOK c

instance HBifunctor CoreChanCMD where
  hbimap _ _ (ReadOne c)          = ReadOne c
  hbimap _ f (ReadChan c n n' a)  = ReadChan c (f n) (f n') a
  hbimap _ f (WriteOne c x)       = WriteOne c (f x)
  hbimap _ f (WriteChan c n n' a) = WriteChan c (f n) (f n') a
  hbimap _ _ (CloseChan c)        = CloseChan c
  hbimap _ _ (ReadOK c)           = ReadOK c

instance (CoreChanCMD :<: instr) => Reexpressible CoreChanCMD instr where
  reexpressInstrEnv reexp (ReadOne c) = lift $ singleInj $ ReadOne c
  reexpressInstrEnv reexp (ReadChan c f t a) = do
      rf <- reexp f
      rt <- reexp t
      lift $ singleInj $ ReadChan c rf rt a
  reexpressInstrEnv reexp (WriteOne c x) = lift . singleInj . WriteOne c =<< reexp x
  reexpressInstrEnv reexp (WriteChan c f t a) = do
      rf <- reexp f
      rt <- reexp t
      lift $ singleInj $ WriteChan c rf rt a
  reexpressInstrEnv reexp (CloseChan c)   = lift $ singleInj $ CloseChan c
  reexpressInstrEnv reexp (ReadOK c)      = lift $ singleInj $ ReadOK c


runCoreChanCMD :: CoreChanCMD (Param3 Run Data PrimType') a -> Run a
runCoreChanCMD (ReadOne (CoreChanRun c)) = Run $ singleInj $ Imp.ReadOne c
runCoreChanCMD (WriteOne (CoreChanRun c) v) = Run $ singleInj $ Imp.WriteOne c v
runCoreChanCMD (ReadChan (CoreChanRun c) off sz arr) = Run $ singleInj $ Imp.ReadChan c off sz arr
runCoreChanCMD (WriteChan (CoreChanRun c) off sz arr) = Run $ singleInj $ Imp.WriteChan c off sz arr
runCoreChanCMD (CloseChan (CoreChanRun c)) = Run $ Imp.closeChan c
runCoreChanCMD (ReadOK (CoreChanRun c)) = Run $ singleInj $ Imp.ReadOK c

instance Interp CoreChanCMD Run (Param2 Data PrimType')
  where interp = runCoreChanCMD
