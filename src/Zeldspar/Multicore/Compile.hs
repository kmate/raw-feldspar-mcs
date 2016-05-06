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
    let next = nextCoreIds ps
    i <- newChan host 0 ichs
    o <- foldParZ ochs i next ps $ \ chs i c n p -> do
        o <- newChan c n chs
        onHost $ onCore c $ translate (p >> return ()) (readChan i) (writeChan o)
        return o
    onHost $ do
        while (return true) $ do
            x <- inp
            writeChan i x
            x <- readChan o
            out x


foldParZ :: (Monad m, Transferable inp, Transferable out)
         => SizeSpec out
         -> c inp
         -> CoreIdTree
         -> MulticoreZ inp out
         -> (forall inp out a. (Transferable inp, Transferable out)
             => SizeSpec out -> c inp -> CoreId -> CoreId -> CoreZ inp out -> m (c out))
         -> m (c out)
foldParZ chs acc (One n)     (OnCore  p c)   f = f chs acc c n p
foldParZ chs acc (Two na nb) (Connect s a b) f = do
    acc' <- foldParZ s acc na a f
    foldParZ chs acc' nb b f


data CoreIdTree = One CoreId | Two CoreIdTree CoreIdTree deriving Show

nextCoreIds :: MulticoreZ inp out -> CoreIdTree
nextCoreIds p = rebuild tree $ shift $ ids $ tree
  where
    tree = create p

create :: MulticoreZ inp out -> CoreIdTree
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
