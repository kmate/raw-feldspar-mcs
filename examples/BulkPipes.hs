{-# LANGUAGE FlexibleContexts #-}
module BulkPipes where

import qualified Prelude

import Feldspar.Multicore


bulkPipes :: Size -> Size -> Size -> Multicore ()
bulkPipes ioChunkSize bufferSize bulkSize = do
    p0 <- allocHostPipe 0 bufferSize
    p1 <- allocCorePipe 0 1 bufferSize
    p2 <- allocHostPipe 1 bufferSize
    onHost $ do
        initPipe p0
        initPipe p1
        initPipe p2
        onCore 0 (f bulkSize p0 p1)
        onCore 1 (g bulkSize p1 p2)

        done <- initRef false
        while (not <$> getRef done) $ do
            input :: Arr Int32 <- newArr $ value ioChunkSize
            for (0, 1, Excl $ value ioChunkSize) $ \i -> do
                item <- lift $ fget stdin
                iff (0 == item)
                    (setRef done true)
                    (return ())
                setArr i item input

            pushPipe p0 (0, value $ ioChunkSize - 1) input
            output <- newArr $ value ioChunkSize
            pullPipe p2 (0, value $ ioChunkSize - 1) output

            for (0, 1, Excl $ value ioChunkSize) $ \i -> do
                item :: Data Int32 <- getArr i output
                printf "> %d\n" item


f :: Size -> HostToCorePipe Int32 -> CorePipe Int32 -> CoreComp ()
f = mapBulk (+1)

g :: Size -> CorePipe Int32 -> CoreToHostPipe Int32 -> CoreComp ()
g = mapBulk (*2)

mapBulk :: (PrimType a, BulkPipeReader ip CoreComp, BulkPipeWriter op CoreComp)
        => (Data a -> Data a) -> Size -> ip a -> op a -> CoreComp ()
mapBulk f bulkSize input output = forever $ do
    tmp <- newArr $ value bulkSize
    pullPipe input (0, value $ bulkSize - 1) tmp
    for (0, 1, Excl $ value bulkSize) $ \i -> do
        elem :: Data a <- getArr i tmp
        setArr i (f elem) tmp
    pushPipe output (0, value $ bulkSize - 1) tmp


------------------------------------------------------------

test = bulkPipes 4 2 2

testAll = do
    icompileAll `onParallella` test
    let modules = compileAll `onParallella` test
    forM_ modules $ \(name, contents) -> do
        let name' = if name Prelude.== "main" then "host" else name
        writeFile (name' Prelude.++ ".c") contents

runTestCompiled = runCompiled' opts test
  where
    opts = defaultExtCompilerOpts {externalFlagsPost = ["-lpthread"]}
