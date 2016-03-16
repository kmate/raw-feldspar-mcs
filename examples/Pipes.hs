module Pipes where

import qualified Prelude

import Feldspar.Multicore


pipes :: Size -> Size -> Multicore ()
pipes ioChunkSize bufferSize = do
    p0 <- allocPipe bufferSize
    p1 <- allocPipe bufferSize
    p2 <- allocPipe bufferSize
    onHost $ do
        initPipe p0
        initPipe p1
        initPipe p2
        onCore (f p0 p1)
        onCore (g p1 p2)

        while (return $ true) $ do
            input :: Arr Int32 <- newArr $ value ioChunkSize
            for (0, 1, Excl $ value ioChunkSize) $ \i -> do
                item <- lift $ fget stdin
                setArr i item input

            pushPipe p0 (0, value $ ioChunkSize - 1) input
            output <- newArr $ value ioChunkSize
            pullPipe p2 (0, value $ ioChunkSize - 1) output

            for (0, 1, Excl $ value ioChunkSize) $ \i -> do
                item :: Data Int32 <- getArr i output
                printf "> %d\n" item


f :: Pipe 0 Int32 -> Pipe 1 Int32 -> CoreComp 0 ()
f input output = forever $ do
    elem <- readPipe input
    writePipe (elem + 1) output

g :: Pipe 1 Int32 -> Pipe 2 Int32 -> CoreComp 1 ()
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
