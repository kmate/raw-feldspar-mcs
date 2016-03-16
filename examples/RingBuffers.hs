module RingBuffers where

import qualified Prelude

import Feldspar.Multicore


ringBuffers :: Size -> Size -> Multicore ()
ringBuffers ioChunkSize bufferSize = do
    b0 <- allocBuff 0 bufferSize
    b1 :: Buffer Int32 <- allocBuff 1 bufferSize
    b2 <- allocBuff 2 bufferSize
    onHost $ do
        initBuff b0
        initBuff b1
        initBuff b2
        onCore 0 (f b0 b1)
        onCore 1 (g b1 b2)

        while (return $ true) $ do
            input :: Arr Int32 <- newArr $ value ioChunkSize
            for (0, 1, Excl $ value ioChunkSize) $ \i -> do
                item :: Data Int32 <- lift $ fget stdin
                setArr i item input

            fetchBuff b0 (0, value $ ioChunkSize - 1) input
            output <- newArr $ value ioChunkSize
            flushBuff b2 (0, value $ ioChunkSize - 1) output

            for (0, 1, Excl $ value ioChunkSize) $ \i -> do
                item :: Data Int32 <- getArr i output
                lift $ printf "> %d\n" item


f :: Buffer Int32 -> Buffer Int32 -> CoreComp ()
f input output = forever $ do
    elem <- readBuff input
    writeBuff (elem + 1) output

g :: Buffer Int32 -> Buffer Int32 -> CoreComp ()
g input output = forever $ do
    elem <- readBuff input
    writeBuff (elem * 2) output


------------------------------------------------------------

test = ringBuffers 3 2

testAll = do
    icompileAll `onParallella` test
    let modules = compileAll `onParallella` test
    forM_ modules $ \(name, contents) -> do
        let name' = if name Prelude.== "main" then "host" else name
        writeFile (name' Prelude.++ ".c") contents

runTestCompiled = runCompiled' opts test
  where
    opts = defaultExtCompilerOpts {externalFlagsPost = ["-lpthread"]}
