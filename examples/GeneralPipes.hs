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
    host_fork   <- allocPipe 0 bufferSize
    fork_inc    <- allocPipe 1 bufferSize
    fork_twice  <- allocPipe 2 bufferSize
    inc_merge   <- allocPipe 3 bufferSize
    twice_merge <- allocPipe 3 bufferSize
    merge_host  <- allocPipe 4 bufferSize
    onHost $ do
        mapM_ initPipe [host_fork, fork_inc, fork_twice, inc_merge, twice_merge, merge_host]
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


fork :: SmallType a => Pipe a -> Pipe a -> Pipe a -> CoreComp ()
fork input out1 out2 = forever $ do
    elem <- readPipe input
    writePipe elem out1
    writePipe elem out2

merge :: SmallType a => Pipe a -> Pipe a
      -> (Data a -> Data a -> Data a) -> Pipe a -> CoreComp ()
merge in1 in2 op output = forever $ do
    elem1 <- readPipe in1
    elem2 <- readPipe in2
    writePipe (elem1 `op` elem2) output


inc :: (SmallType a, Num a) => Pipe a -> Pipe a -> CoreComp ()
inc input output = forever $ do
    elem <- readPipe input
    writePipe (elem + 1) output

twice :: (SmallType a, Num a) => Pipe a -> Pipe a -> CoreComp ()
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
