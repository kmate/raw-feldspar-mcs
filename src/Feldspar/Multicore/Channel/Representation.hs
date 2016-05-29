{-# LANGUAGE UndecidableInstances #-}
module Feldspar.Multicore.Channel.Representation where

import Control.Monad.Operational.Higher
import Control.Monad.Trans
import Data.Ix
import Data.Proxy
import Data.Typeable
import Data.TypedStruct
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
  | CoreChanComp ChanCompRep

data ChanCompRep
  = HostChanRep (FunArg Data PrimType')
  | CoreChanRep (FunArg Data PrimType') (FunArg Data PrimType') (FunArg Data PrimType')


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
  ReadChan  :: (Typeable a, pred a)
            => CoreChan c -> exp Index -> exp Index
            -> Arr a -> CoreChanCMD (Param3 prog exp pred) (exp Bool)

  WriteOne  :: (Typeable a, pred a)
            => CoreChan a -> exp a
            -> CoreChanCMD (Param3 prog exp pred) (exp Bool)

  WriteChan :: (Typeable a, pred a)
            => CoreChan c -> exp Index -> exp Index
            -> Arr a -> CoreChanCMD (Param3 prog exp pred) (exp Bool)

  CloseChan :: CoreChan a -> CoreChanCMD (Param3 prog exp pred) ()


instance HFunctor CoreChanCMD where
  hfmap _ (ReadChan c f t a)  = ReadChan c f t a
  hfmap _ (WriteOne c x)      = WriteOne c x
  hfmap _ (WriteChan c f t a) = WriteChan c f t a
  hfmap _ (CloseChan c)       = CloseChan c

runCoreChanCMD :: CoreChanCMD (Param3 Run Data PrimType') a -> Run a
runCoreChanCMD (WriteOne (CoreChanRun c) v) =
    Run $ (fmap Imp.valToExp) $ singleInj $ Imp.WriteOne c v
runCoreChanCMD (ReadChan (CoreChanRun c) off sz (Arr _ (Single arr))) =
    Run $ (fmap Imp.valToExp) $ singleInj $ Imp.ReadChan c off sz arr
runCoreChanCMD (WriteChan (CoreChanRun c) off sz (Arr _ (Single arr))) =
    Run $ (fmap Imp.valToExp) $ singleInj $ Imp.WriteChan c off sz arr
runCoreChanCMD (CloseChan (CoreChanRun c)) = Run $ Imp.closeChan c

instance Interp CoreChanCMD Run (Param2 Data PrimType')
  where interp = runCoreChanCMD
