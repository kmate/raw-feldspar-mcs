{-# LANGUAGE AllowAmbiguousTypes#-}
module Feldspar.Multicore.Channel where

import Feldspar hiding ((==))
import Feldspar.Multicore.Frontend
import Feldspar.Multicore.Pipe
import Feldspar.Multicore.Representation


host :: CoreId
host = sharedId

data Chan a where
    HostToCoreChan :: ChanType a => HostToCorePipe (ChanElemType a) -> Chan a
    CoreToHostChan :: ChanType a => CoreToHostPipe (ChanElemType a) -> Chan a
    CoreToCoreChan :: ChanType a => CorePipe       (ChanElemType a) -> Chan a

class ChanType a
  where
    type ChanElemType a :: *
    newChan :: CoreId -> CoreId -> Length -> Multicore (Chan a)

class ChanType a => Transferable m a
  where
    readChan :: Chan a -> m a
    writeChan :: Chan a -> a -> m ()

instance PrimType a => ChanType (Data a)
  where
    type ChanElemType (Data a) = a
    newChan from to l
        | host == from = do
            p :: HostToCorePipe a <- allocHostPipe to l
            onHost $ initPipe p
            return $ HostToCoreChan p
        | host == to   = do
            p :: CoreToHostPipe a <- allocHostPipe from l
            onHost $ initPipe p
            return $ CoreToHostChan p
        | otherwise = do
            p <- allocCorePipe from to l
            onHost $ initPipe p
            return $ CoreToCoreChan p

instance PrimType a => Transferable Host (Data a)
  where
    readChan (CoreToHostChan p) = do
        arr <- newArr 1
        pullPipe p (0,0) arr
        elem <- getArr 0 arr
        return elem
    writeChan (HostToCoreChan p) v = do
        arr <- newArr 1
        setArr 0 v arr
        pushPipe p (0,0) arr

instance PrimType a => Transferable CoreComp (Data a)
  where
    readChan (HostToCoreChan p) = readPipe p
    readChan (CoreToCoreChan p) = readPipe p
    writeChan (CoreToHostChan p) v = writePipe v p
    writeChan (CoreToCoreChan p) v = writePipe v p
