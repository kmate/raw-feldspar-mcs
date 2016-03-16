module Pipes where

import qualified Prelude

import Feldspar.Multicore


pipes :: Size -> Size -> Multicore ()
pipes ioChunkSize bufferSize = do
    p0 <- allocPipe 0 bufferSize
    p1 <- allocPipe 1 bufferSize
    p2 <- allocPipe 2 bufferSize
    onHost $ do
        initPipe p0
        initPipe p1
        initPipe p2
        onCore 0 (f p0 p1)
        onCore 1 (g p1 p2)

        while (return $ true) $ do
            input :: Arr Int32 <- newArr $ value ioChunkSize
            for (0, 1, Excl $ value ioChunkSize) $ \i -> do
                item :: Data Int32 <- lift $ fget stdin
                setArr i item input

            pushPipe p0 (0, value $ ioChunkSize - 1) input
            output <- newArr $ value ioChunkSize
            pullPipe p2 (0, value $ ioChunkSize - 1) output

            for (0, 1, Excl $ value ioChunkSize) $ \i -> do
                item :: Data Int32 <- getArr i output
                lift $ printf "> %d\n" item


f :: Pipe Int32 -> Pipe Int32 -> CoreComp ()
f input output = forever $ do
    elem <- readPipe input
    writePipe (elem + 1) output

g :: Pipe Int32 -> Pipe Int32 -> CoreComp ()
g input output = forever $ do
    elem <- readPipe input
    writePipe (elem * 2) output


------------------------------------------------------------

test = pipes 3 2

testAll = do
    icompileAll `onParallella` test
    let modules = compileAll `onParallella` test
    forM_ modules $ \(name, contents) -> do
        let name' = if name Prelude.== "main" then "host" else name
        writeFile (name' Prelude.++ ".c") contents

runTestCompiled = runCompiled' opts test
  where
    opts = defaultExtCompilerOpts {externalFlagsPost = ["-lpthread"]}
