module Zeldspar.Multicore.Compile where

import Control.Monad.Trans as C
import Prelude hiding ((&&))

import Feldspar.Data.Option
import Feldspar.Run.Concurrent (waitThread)
import Feldspar.Multicore
import Feldspar.Multicore.Representation hiding (OnCore)

import Zeldspar (translate)
import Zeldspar.Multicore.Representation


runZ :: forall inp inp' out out' a
     .  ( Transferable inp, TransferType Host inp' inp
        , Transferable out, TransferType Host out' out )
     => MulticoreZ inp out a
     -> (Host (inp', Data Bool))    -- ^ Source
     -> SizeSpec inp                -- ^ Source channel size
     -> (out' -> Host (Data Bool))  -- ^ Sink
     -> SizeSpec out                -- ^ Sink channel size
     -> Multicore ()
runZ ps inp ichs out ochs = do
    let next = nextCoreIds ps
    i <- newChan host 0 ichs
    o <- foldParZ ochs i next ps $ \ chs i c n p -> do
        o <- newChan c n chs
        onHost $ onCore c $ translate (void p) (coreRead i o) (coreWrite i o)
        return o
    onHost $ do
        -- Read from output channel, shove output into sink
        outThread <- forkWithId $ \_ -> do
            continue <- initRef true
            while (getRef continue) $ do
                s <- newSlot o
                isOpen <- readChan o s
                iff isOpen
                    (do x <- getSlot s
                        x' <- fromTransfer x
                        dontStop <- out x'
                        setRef continue dontStop
                        iff dontStop (return ()) (closeChan o))
                    (setRef continue false)

        -- Read from source, shove into input channel
        continue <- initRef true
        while (getRef continue) $ do
            (x, dontStop) <- inp
            iff dontStop
                (do  x' <- toTransfer x
                     isOpen <- writeChan i x'
                     setRef continue isOpen)
                (setRef continue false)
        closeChan i
        lift $ waitThread outThread
    where
        coreRead i o = do
            s <- newSlot i
            isOpen <- readChan i s
            iff isOpen (return ()) (closeChan o)
            getSlot s

        coreWrite i o v = do
            isOpen <- writeChan o v
            iff isOpen (return ()) (closeChan i)


foldParZ :: (Monad m, Transferable inp, Transferable out)
         => SizeSpec out
         -> c inp
         -> CoreIdTree
         -> MulticoreZ inp out a
         -> (forall inp out a. (Transferable inp, Transferable out)
             => SizeSpec out -> c inp -> CoreId -> CoreId -> CoreZ inp out a -> m (c out))
         -> m (c out)
foldParZ chs acc (One n)     (OnCore  p c)   f = f chs acc c n p
foldParZ chs acc (Two na nb) (Connect s a b) f = do
    acc' <- foldParZ s acc na a f
    foldParZ chs acc' nb b f


data CoreIdTree = One CoreId | Two CoreIdTree CoreIdTree deriving Show

nextCoreIds :: MulticoreZ inp out a -> CoreIdTree
nextCoreIds p = rebuild tree $ shift $ ids $ tree
  where
    tree = create p

create :: MulticoreZ inp out a -> CoreIdTree
create (OnCore  _ c)   = One c
create (Connect _ a b) = Two (create a) (create b)

ids :: CoreIdTree -> [CoreId]
ids (One c)   = [c]
ids (Two a b) = ids a ++ ids b

shift :: [CoreId] -> [CoreId]
shift (_:cs) = cs ++ [host]

rebuild :: CoreIdTree -> [CoreId] -> CoreIdTree
rebuild (One _)  [c] = One c
rebuild (Two a b) cs =
    let (as, bs) = Prelude.splitAt (count a) cs
    in  Two (rebuild a as) (rebuild b bs)

count :: CoreIdTree -> Int
count (One _)   = 1
count (Two a b) = count a + count b
