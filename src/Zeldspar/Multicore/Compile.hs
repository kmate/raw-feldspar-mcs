{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Zeldspar.Multicore.Compile where

import Feldspar.Multicore
import Feldspar.Multicore.Representation hiding (OnCore)
import Zeldspar.Multicore.Representation


import Data.Typeable

import Debug.Trace

{-
-- FIXME: use something else related to Transferable instead of SmallType?
parZ :: forall inp out. (SmallType inp, SmallType out)
     => ParZ inp out ()
     -> Host (Data inp, Data Bool)      -- ^ Source
     -> (Data out -> Host (Data Bool))  -- ^ Sink
     -> Multicore ()
parZ ps inp out= do
    i :: HostToCorePipe inp <- allocHostPipe 0 10
    o :: CoreToHostPipe out <- allocHostPipe 1 10
    onHost $ do
        initPipe i
        initPipe o
        
        undefined -- TODO: implement the translation with Pipes
        -- use foldPP from Ziria.Parallel if possible
-}

translatePar :: forall inp out. (CoreTransferable inp, CoreTransferable out)
             => MulticoreZ inp out
             -> (Host (inp, Data Bool))    -- ^ Source
             -> (out -> Host (Data Bool))  -- ^ Sink
             -> Multicore ()
translatePar ps inp out = do
    i <- newChan (Proxy :: Proxy inp) sharedId 0 10
    let last = parSize ps - 1
        ids  = Prelude.zip [0..last] ([1..last] ++ [sharedId])
{-
    foldParZ 10 i ps $ \chs i p -> do
      -- o <- allocCorePipe 0 1 chs
      onHost $ onCore 0 $ void $ do return ()
      --  translate (p >> return ())
          --  (error "read i") -- (readC t i o)
          --  (error "write o")-- (writeC t i o)
      --  closeChan i
      --  closeChan o
      onHost $ printf "at least folds\n" -- o
      return $ error "this"
    -- o :: CoreToHostPipe (Internal out) <- allocHostPipe 1 10
-}
    traceShow ids $ onHost $ do
--        initPipe i
        -- initPipe o
        printf "at least does something\n"
--    error "unfinished"

-- class    (Syntax a, PrimType (Internal a), Transferable a) => CoreTransferable a
--instance (Syntax a, PrimType (Internal a), Transferable a) => CoreTransferable a

data CoreChan a = forall p. Pipe p => CoreChan (p a)

class CoreTransferable a
  where
    type ChanElemType a :: *

    newChan :: proxy a
            -> CoreId
            -> CoreId
            -> Length
            -> Multicore (CoreChan (ChanElemType a))

instance PrimType a => CoreTransferable (Data a)
  where
    type ChanElemType (Data a) = a
    newChan _ from to l
        | sharedId Prelude.== from = do
            p :: HostToCorePipe a <- allocHostPipe to l
            onHost $ initPipe p
            return $ CoreChan p
        | sharedId Prelude.== to   = do
            p :: CoreToHostPipe a <- allocHostPipe from l
            onHost $ initPipe p
            return $ CoreChan p
        | otherwise = do
            p <- allocCorePipe from to l
            onHost $ initPipe p
            return $ CoreChan p

parSize :: MulticoreZ inp out -> Length
parSize (OnCore _ _)    = 1
parSize (Connect _ a b) = parSize a + parSize b

{-
foldParZ :: ( CoreTransferable inp, CoreTransferable out )
         => Length
         -> HostToCorePipe (Internal inp)
         -> ParZ inp out m a
         -> (forall pi po inp out a
             .  ( Pipe pi, Pipe po
                , CoreTransferable inp, CoreTransferable out )
             => Length
             -> pi (Internal inp)
             -> Z inp out m a
             -> Multicore (po (Internal out)))
         -> Multicore (CoreToHostPipe (Internal out))
foldParZ chs acc (LiftP p)     f = f chs acc p
foldParZ chs acc (ConnP s a b) f = do
    acc' <- foldParZ {-s-} 0 acc a f
    let acc'' = undefined
    foldParZ chs acc'' b f
-}

{-
-- TODO: do the same thing as Zeldspar.Paralllel did, just pass Pipes instead of chans

-- | Left fold over a 'ParZ'
foldParZ :: (Monad m, Transferable inp, Transferable out)
         => SizeSpec out
         -> c inp
         -> ParZ inp out m a
         -> (forall inp out a. (Transferable inp, Transferable out)
             => SizeSpec out -> c inp -> Z inp out m a -> m (c out))
         -> m (c out)
foldParZ chs acc (LiftP p)     f = f chs acc p
foldParZ chs acc (ConnP s a b) f = foldParZ s acc a f >>= \acc' -> foldParZ chs acc' b f
-}
