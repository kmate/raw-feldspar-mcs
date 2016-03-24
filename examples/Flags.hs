module Flags where

import qualified Prelude

import Feldspar.Multicore


n :: Word32
n = 4

flags :: Multicore ()
flags = do
    f0 <- allocRef 0
    b0 <- allocLArr 0 n
    f1 <- allocRef 1
    b1 <- allocLArr 1 n
    f2 <- allocRef 2
    b2 <- allocLArr 2 n
    onHost $ do
        setLocalRef f0 false
        setLocalRef f1 false
        setLocalRef f2 false
        onCore 0 (f (f0, b0) (f1, b1))
        onCore 1 (g (f1, b1) (f2, b2))

        while (return $ true) $ do
            input :: Arr Int32 <- newArr $ value n
            for (0, 1, Excl $ value n) $ \i -> do
                item <- lift $ fget stdin
                setArr i item input

            writeArr b0 (0, value $ n - 1) input
            setLocalRef f0 true
            output <- newArr $ value n
            while (not <$> getLocalRef f2) $ return ()
            setLocalRef f2 false
            readArr b2 (0, value $ n - 1) output

            for (0, 1, Excl $ value n) $ \i -> do
                item :: Data Int32 <- getArr i output
                printf "> %d\n" item


f :: (LocalRef Bool, LocalArr Int32) -> (LocalRef Bool, LocalArr Int32) -> CoreComp ()
f (ri, input) (ro, output) = forever $ do
    while (not <$> getLocalRef ri) $ return ()
    setLocalRef ri false

    for (0, 1, Excl $ value n) $ \i -> do
        item :: Data Int32 <- getArr i -< input
        setArr i (item + 1) -< output
    setLocalRef ro true

g :: (LocalRef Bool, LocalArr Int32) -> (LocalRef Bool, LocalArr Int32) -> CoreComp ()
g (ri, input) (ro, output) = forever $ do
    while (not <$> getLocalRef ri) $ return ()
    setLocalRef ri false

    tmp <- newArr 10
    readArr input (0,9) tmp
    for (0, 1, Incl 9) $ \i -> do
        item :: Data Int32 <- getArr i tmp
        setArr i (item * 2) tmp
    writeArr output (0,9) tmp
    setLocalRef ro true


------------------------------------------------------------

test = flags

testAll = do
    icompileAll `onParallella` test
    let modules = compileAll `onParallella` test
    forM_ modules $ \(name, contents) -> do
        let name' = if name Prelude.== "main" then "host" else name
        writeFile (name' Prelude.++ ".c") contents

runTestCompiled = runCompiled' opts test
  where
    opts = defaultExtCompilerOpts {externalFlagsPost = ["-lpthread"]}
