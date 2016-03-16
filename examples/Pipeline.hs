module Pipeline where

import qualified Prelude

import Feldspar.Multicore


n :: Word32
n = 4

pipeline :: Multicore ()
pipeline = do
    r0 :: Arr Bool <- alloc 0 1
    a0 <- alloc 0 n
    r1 :: Arr Bool <- alloc 1 1
    a1 <- alloc 1 n
    r2 :: Arr Bool <- alloc 2 1
    a2 <- alloc 2 n
    onHost $ do
        check <- initArr [False]
        reset <- initArr [False]
        set <- initArr [True]
        fetch r0 (0,0) reset
        fetch r1 (0,0) reset
        fetch r2 (0,0) reset
        onCore 0 (f (r0, a0) (r1, a1))
        onCore 1 (g (r1, a1) (r2, a2))

        while (return $ true) $ do
            input :: Arr Int32 <- newArr $ value n
            for (0, 1, Excl $ value n) $ \i -> do
                item :: Data Int32 <- lift $ fget stdin
                setArr i item input

            fetch a0 (0, value $ n - 1) input
            fetch r0 (0,0) set
            output <- newArr $ value n
            while (not <$> (flush r2 (0,0) check >> getArr 0 check)) $ return ()
            fetch r2 (0,0) reset
            flush a2 (0, value $ n - 1) output

            for (0, 1, Excl $ value n) $ \i -> do
                item :: Data Int32 <- getArr i output
                lift $ printf "> %d\n" item


f :: (Arr Bool, Arr Int32) -> (Arr Bool, Arr Int32) -> Comp ()
f (ri, input) (ro, output) = while (return $ true) $ do
    while (not <$> getArr 0 ri) $ return ()
    setArr 0 false ri

    for (0, 1, Excl $ value n) $ \i -> do
        item :: Data Int32 <- getArr i input
        setArr i (item + 1) output
    setArr 0 true ro

g :: (Arr Bool, Arr Int32) -> (Arr Bool, Arr Int32) -> Comp ()
g (ri, input) (ro, output) = while (return $ true) $ do
    while (not <$> getArr 0 ri) $ return ()
    setArr 0 false ri

    for (0, 1, Excl $ value n) $ \i -> do
        item :: Data Int32 <- getArr i input
        setArr i (item * 2) output
    setArr 0 true ro


------------------------------------------------------------

testAll = icompileAll `onParallella` pipeline

runTestCompiled = runCompiled' opts pipeline
  where
    opts = defaultExtCompilerOpts {externalFlagsPost = ["-lpthread"]}
