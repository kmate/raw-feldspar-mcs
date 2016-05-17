module ZeldsparVector where

import qualified Prelude

import Zeldspar.Multicore


vecInc :: (PrimType a, Num a) => CoreZ (Vector (Data a)) (Vector (Data a)) ()
vecInc = loop $ do
    s <- receive'
    v <- lift $ unsafeFreezeStore s
    lift $ writeStore s (map (+1) v)
    emit' s

vecRev :: PrimType a => CoreZ (Vector (Data a)) (Vector (Data a)) ()
vecRev = loop $ do
    v <- receive
    emit (reverse v)


vector :: Multicore ()
vector = runZ
    (vecInc `on` 0 |>>chanSize>>| vecRev `on` 1)
    readInput
    chanSize
    writeOutput
    chanSize
  where
    vecSize  = 5
    chanSize = 10`ofLength`vecSize
    readInput :: Host (Vector (Data Int32), Data Bool)
    readInput = liftHost $ do
        input <- newArr $ value vecSize
        for (0, 1, Excl $ value vecSize) $ \i -> do
            v <- fget stdin
            setArr i v input
        vec <- unsafeFreezeVec (value $ vecSize) input
        return (vec, true)
    writeOutput :: Vector (Data Int32) -> Host (Data Bool)
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

runTestCompiled = runCompiled' opts test
  where
    opts = defaultExtCompilerOpts {externalFlagsPost = ["-lpthread"]}
