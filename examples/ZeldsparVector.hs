module ZeldsparVector where

import qualified Prelude

import Zeldspar.Multicore


vecInc :: (PrimType a, Num a) => CoreZ (Store (DPull a)) (Store (DPull a)) ()
vecInc = loop $ do
    s <- take
    v <- lift $ unsafeFreezeStore s
    lift $ writeStore s (fmap (+1) v)
    emit s

vecInc' :: (PrimType a, Num a) => CoreZ (DPull a) (Store (DPull a)) ()
vecInc' = loop $ do
    v <- take
    s <- lift $ initStore (fmap (+1) v)
    emit s

vecInc'' :: (PrimType a, Num a) => CoreZ (Store (DPull a)) (DPull a) ()
vecInc'' = loop $ do
    s <- take
    v <- lift $ unsafeFreezeStore s
    emit (fmap (+1) v)


vecTwice :: (PrimType a, Num a) => CoreZ (DPull a) (DPull a) ()
vecTwice = loop $ do
    v <- take
    emit $ fmap (*2) v

vecRev :: PrimType a => CoreZ (DPull a) (DPull a) ()
vecRev = loop $ do
    v <- take
    emit (reverse v)


vector :: Multicore ()
vector = runParZ
    ((vecInc `on` 0) |>>chanSize>>|
     (vecInc' `on` 1) |>>chanSize>>|
     (vecInc'' `on` 2) |>>chanSize>>|
     (vecTwice >>> vecRev) `on` 3)
    readInput
    chanSize
    writeOutput
    chanSize
  where
    vecSize = 5
    chanSize = 5
    readInput :: Host (DPull Int32, Data Bool)
    readInput = liftHost $ do
        input <- newArr $ value vecSize
        for (0, 1, Excl $ value vecSize) $ \i -> do
            v <- fget stdin
            setArr input i v
        vec <- unsafeFreezeVec input
        return (vec, true)
    writeOutput :: DPull Int32 -> Host (Data Bool)
    writeOutput o = do
        for (0, 1, Excl $ value vecSize) $ \i -> printf "> %d\n" (o ! i)
        return true


------------------------------------------------------------

test = vector

testAll = do
    icompileAll `onParallella` test
    let modules = compileAll `onParallella` test
    forM_ modules $ \(name, contents) -> do
        let name' = if name Prelude.== "main" then "host" else name
        writeFile (name' Prelude.++ ".c") contents

runTestCompiled = runCompiled' def opts test
  where
    opts = def
        { externalFlagsPre  = [ "-I../imperative-edsl/include"
                              , "../imperative-edsl/csrc/chan.c"]
        , externalFlagsPost = [ "-lpthread" ]
        }

unsafeFreezeVec :: (PrimType a, MonadComp m) => DArr a -> m (DPull a)
unsafeFreezeVec arr = do
  iarr <- unsafeFreezeArr arr
  return $ toPull iarr

