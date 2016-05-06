module Zeldspar.Multicore.Compile where

import Feldspar.Multicore
import Feldspar.Multicore.Representation hiding (OnCore)

import Zeldspar hiding (lift)
import Zeldspar.Multicore.Representation


translatePar :: forall inp out. (Transferable inp, Transferable out)
             => MulticoreZ inp out
             -> (Host inp)        -- ^ Source
             -> SizeSpec inp      -- ^ Source channel size
             -> (out -> Host ())  -- ^ Sink
             -> SizeSpec out      -- ^ Sink channel size
             -> Multicore ()
translatePar ps inp ichs out ochs = do
    i <- newChan host 0 ichs
    o <- foldParZ ochs i host ps $ \ chs i c n p -> do
        o <- newChan c n chs
        onHost $ onCore c $ translate (p >> return ()) (readChan i) (writeChan o)
        return o
    onHost $ do
        while (return true) $ do
            x <- inp
            writeChan i x
            x <- readChan o
            out x

-- TODO: handle next core id for channel creation correctly on fold below

foldParZ :: (Monad m, Transferable inp, Transferable out)
         => SizeSpec out
         -> c inp
         -> CoreId
         -> MulticoreZ inp out
         -> (forall inp out a. (Transferable inp, Transferable out)
             => SizeSpec out -> c inp -> CoreId -> CoreId -> CoreZ inp out -> m (c out))
         -> m (c out)
foldParZ chs acc next (OnCore p c)    f = f chs acc c next p
foldParZ chs acc next (Connect s a b) f = do
    acc' <- foldParZ s acc next a f
    foldParZ chs acc' next b f
