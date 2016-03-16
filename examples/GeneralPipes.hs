module GeneralPipes where

import qualified Prelude

import GHC.TypeLits

import Feldspar.Multicore


{- The cores are connected like this:

       /-- inc ----\
-- fork             merge (+) --
       \-- twice --/

-}
generalPipes :: Size -> Size -> Multicore ()
generalPipes ioChunkSize bufferSize = do
    host_fork   <- allocPipe bufferSize
    fork_inc    <- allocPipe bufferSize
    fork_twice  <- allocPipe bufferSize
    inc_merge   <- allocPipe bufferSize
    twice_merge <- allocPipe bufferSize
    merge_host :: Pipe 4 Int32 <- allocPipe bufferSize
    onHost $ do
        initPipe host_fork
        initPipe fork_inc
        initPipe fork_twice
        initPipe inc_merge
        initPipe twice_merge
        initPipe merge_host
        onCore (fork host_fork fork_inc fork_twice :: CoreComp 0 ())
        onCore (inc fork_inc inc_merge :: CoreComp 1 ())
        onCore (twice fork_twice twice_merge :: CoreComp 2 ())
        onCore (merge inc_merge twice_merge (+) merge_host :: CoreComp 3 ())

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


fork :: (KnownNat cin, KnownNat cout1, KnownNat cout2, SmallType a)
     => Pipe cin a -> Pipe cout1 a -> Pipe cout2 a -> CoreComp cin ()
fork input out1 out2 = forever $ do
    elem <- readPipe input
    writePipe elem out1
    writePipe elem out2

merge :: (KnownNat cin, KnownNat cout, SmallType a)
      => Pipe cin a -> Pipe cin a -> (Data a -> Data a -> Data a)
      -> Pipe cout a -> CoreComp cin ()
merge in1 in2 op output = forever $ do
    elem1 <- readPipe in1
    elem2 <- readPipe in2
    writePipe (elem1 `op` elem2) output


inc :: (KnownNat cin, KnownNat cout, SmallType a, Num a)
    => Pipe cin a -> Pipe cout a -> CoreComp cin ()
inc input output = forever $ do
    elem <- readPipe input
    writePipe (elem + 1) output

twice :: (KnownNat cin, KnownNat cout, SmallType a, Num a)
      => Pipe cin a -> Pipe cout a -> CoreComp cin ()
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
