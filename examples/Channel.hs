module Channel where

import Feldspar.Multicore


channels :: Size -> Size -> Multicore ()
channels ioChunkSize chanSize = do
    c0 <- newChan host 0    chanSize
    c1 <- newChan 0    1    chanSize
    c2 <- newChan 1    host chanSize
    onHost $ do
        onCore 0 (f c0 c1)
        onCore 1 (g c1 c2)

        while (return $ true) $ do
            for (0, 1, Excl $ value ioChunkSize) $ \i -> do
                item <- lift $ fget stdin
                writeChan c0 item

            for (0, 1, Excl $ value ioChunkSize) $ \i -> do
                item :: Data Int32 <- readChan c2
                printf "> %d\n" item


f :: Chan (Data Int32) -> Chan (Data Int32) -> CoreComp ()
f input output = forever $ do
    elem <- readChan input
    writeChan output (elem + 1)

g :: Chan (Data Int32) -> Chan (Data Int32) -> CoreComp ()
g input output = forever $ do
    elem <- readChan input
    writeChan output (elem * 2)


------------------------------------------------------------

test = channels 3 2

testAll = do
    icompileAll `onParallella` test
    let modules = compileAll `onParallella` test
    forM_ modules $ \(name, contents) -> do
        let name' = if name Prelude.== "main" then "host" else name
        writeFile (name' Prelude.++ ".c") contents

runTestCompiled = runCompiled' opts test
  where
    opts = defaultExtCompilerOpts {externalFlagsPost = ["-lpthread"]}
