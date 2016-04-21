{-# LANGUAGE FlexibleContexts #-}
module GeneralPipes where

import qualified Prelude

import Feldspar.Multicore


{- The cores are connected like this:

       /-- inc ----\
-- fork             merge (+) --
       \-- twice --/

-}
generalPipes :: Size -> Size -> Multicore ()
generalPipes ioChunkSize bufferSize = do
    host_fork :: HostToCorePipe Int32 <- allocHostPipe 0 bufferSize
    fork_inc    <- allocCorePipe 0 1 bufferSize
    fork_twice  <- allocCorePipe 1 2 bufferSize
    inc_merge   <- allocCorePipe 2 3 bufferSize
    twice_merge <- allocCorePipe 1 3 bufferSize
    merge_host :: CoreToHostPipe Int32 <- allocHostPipe 3 bufferSize
    onHost $ do
        initPipe host_fork
        initPipe fork_inc
        initPipe fork_twice
        initPipe inc_merge
        initPipe twice_merge
        initPipe merge_host
        onCore 0 (fork host_fork fork_inc fork_twice)
        onCore 1 (inc fork_inc inc_merge)
        onCore 2 (twice fork_twice twice_merge)
        onCore 3 (merge inc_merge twice_merge (+) merge_host)

        while (return $ true) $ do
            input :: Arr Int32 <- newArr $ value ioChunkSize
            for (0, 1, Excl $ value ioChunkSize) $ \i -> do
                item <- lift $ fget stdin
                setArr i item input

            pushPipe host_fork (0, value $ ioChunkSize - 1) input
            output <- newArr $ value ioChunkSize
            pullPipe merge_host (0, value $ ioChunkSize - 1) output

            for (0, 1, Excl $ value ioChunkSize) $ \i -> do
                item :: Data Int32 <- getArr i output
                printf "> %d\n" item


fork :: (PrimType a, PipeReader ip CoreComp, PipeWriter op1 CoreComp, PipeWriter op2 CoreComp)
     => ip a -> op1 a -> op2 a -> CoreComp ()
fork input out1 out2 = forever $ do
    elem <- readPipe input
    writePipe elem out1
    writePipe elem out2

merge :: (PrimType a, PipeReader ip1 CoreComp, PipeReader ip2 CoreComp, PipeWriter op CoreComp)
      => ip1 a -> ip2 a -> (Data a -> Data a -> Data a) -> op a -> CoreComp ()
merge in1 in2 op output = forever $ do
    elem1 <- readPipe in1
    elem2 <- readPipe in2
    writePipe (elem1 `op` elem2) output


inc :: (PrimType a, Num a, PipeReader ip CoreComp, PipeWriter op CoreComp)
    => ip a -> op a -> CoreComp ()
inc input output = forever $ do
    elem <- readPipe input
    writePipe (elem + 1) output

twice :: (PrimType a, Num a, PipeReader ip CoreComp, PipeWriter op CoreComp)
      => ip a -> op a -> CoreComp ()
twice input output = forever $ do
    elem <- readPipe input
    writePipe (elem * 2) output


------------------------------------------------------------

test = generalPipes 3 2

testAll = do
    icompileAll `onParallella` test
    let modules = compileAll `onParallella` test
    forM_ modules $ \(name, contents) -> do
        let name' = if name Prelude.== "main" then "host" else name
        writeFile (name' Prelude.++ ".c") contents

runTestCompiled = runCompiled' opts test
  where
    opts = defaultExtCompilerOpts {externalFlagsPost = ["-lpthread"]}
