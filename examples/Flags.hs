module Flags where

import qualified Prelude

import Feldspar.Multicore


n :: Word32
n = 4

flags :: Multicore ()
flags = do
    f0 <- allocRef
    b0 <- allocArr n
    f1 <- allocRef
    b1 <- allocArr n
    f2 <- allocRef
    b2 <- allocArr n
    onHost $ do
        writeRef f0 false
        writeRef f1 false
        writeRef f2 false
        onCore (f (f0, b0) (f1, b1))
        onCore (g (f1, b1) (f2, b2))

        while (return $ true) $ do
            input :: Arr Int32 <- newArr $ value n
            for (0, 1, Excl $ value n) $ \i -> do
                item <- lift $ fget stdin
                setArr i item input

            writeArr b0 (0, value $ n - 1) input
            writeRef f0 true
            output <- newArr $ value n
            while (not <$> readRef f2) $ return ()
            writeRef f2 false
            readArr b2 (0, value $ n - 1) output

            for (0, 1, Excl $ value n) $ \i -> do
                item :: Data Int32 <- getArr i output
                printf "> %d\n" item


f :: (LocalRef 0 Bool, LocalArr 0 Int32)
  -> (LocalRef 1 Bool, LocalArr 1 Int32) -> CoreComp 0 ()
f (ri, input) (ro, output) = forever $ do
    while (not <$> getLocalRef ri) $ return ()
    setLocalRef ri false

    for (0, 1, Excl $ value n) $ \i -> do
        item :: Data Int32 <- getArr i -< input
        setArr i (item + 1) -< output
    setLocalRef ro true

g :: (LocalRef 1 Bool, LocalArr 1 Int32)
  -> (LocalRef 2 Bool, LocalArr 2 Int32) -> CoreComp 1 ()
g (ri, input) (ro, output) = forever $ do
    while (not <$> getLocalRef ri) $ return ()
    setLocalRef ri false

    for (0, 1, Excl $ value n) $ \i -> do
        item :: Data Int32 <- getArr i -< input
        setArr i (item * 2) -< output
    setLocalRef ro true


------------------------------------------------------------

testAll = icompileAll `onParallella` flags

runTestCompiled = runCompiled' opts flags
  where
    opts = defaultExtCompilerOpts {externalFlagsPost = ["-lpthread"]}
