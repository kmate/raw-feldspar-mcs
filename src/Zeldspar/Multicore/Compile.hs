module Zeldspar.Multicore.Compile where

import Control.Monad.Trans as C
import Prelude hiding ((&&))

import Feldspar.Data.Option
import Feldspar.Run.Concurrent (waitThread)
import Feldspar.Multicore
import Feldspar.Multicore.Representation hiding (OnCore)

import Zeldspar (runZ)
import Zeldspar.Multicore.Representation


runParZ :: forall inp inp' out out' a
        .  ( CoreTransferable inp, CoreTransferType Host inp' inp
           , CoreTransferable out, CoreTransferType Host out' out )
        => MulticoreZ inp out a
        -> (Host (inp', Data Bool))    -- ^ Source
        -> SizeSpec inp                -- ^ Source channel size
        -> (out' -> Host (Data Bool))  -- ^ Sink
        -> SizeSpec out                -- ^ Sink channel size
        -> Multicore ()
runParZ ps inp ichs out ochs = do
    let next = nextCoreIds ps
    i <- newChan hostId 0 ichs
    o <- foldParZ ochs i next ps $ \ chs i c n p -> do
        o <- newChan c n chs
        onHost $ onCoreWithRef c $ \r ->
            runZ (void p) (coreRead r i o) (coreWrite r i o)
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
        coreRead r i o = do
            s <- newSlot i
            isOpen <- readChan i s
            iff isOpen (return ()) (closeChan o >> haltCore r)
            getSlot s

        coreWrite r i o v = do
            isOpen <- writeChan o v
            iff isOpen (return ()) (closeChan i >> haltCore r)


foldParZ :: (Monad m, CoreTransferable inp, CoreTransferable out)
         => SizeSpec out
         -> c inp
         -> CoreIdTree
         -> MulticoreZ inp out a
         -> (forall inp out a. (CoreTransferable inp, CoreTransferable out)
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
shift (_:cs) = cs ++ [hostId]

rebuild :: CoreIdTree -> [CoreId] -> CoreIdTree
rebuild (One _)  [c] = One c
rebuild (Two a b) cs =
    let (as, bs) = Prelude.splitAt (count a) cs
    in  Two (rebuild a as) (rebuild b bs)

count :: CoreIdTree -> Int
count (One _)   = 1
count (Two a b) = count a + count b
