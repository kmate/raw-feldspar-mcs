module Benchmark where

import qualified Prelude
import System.Directory

import Feldspar.Multicore


type Benchmark = Size -> Size -> Multicore ()

pattern0 :: Benchmark
pattern0 ioChunkSize bulkSize = do
    ph_0 :: HostToCorePipe Int32 <- allocHostPipe 0 ioChunkSize
    p0_h <- allocHostPipe 0 ioChunkSize
    onHost $ do
        initPipe ph_0
        initPipe p0_h
        let f :: ( Num a, SmallType a
                 , BulkPipeReader ip CoreComp, BulkPipeWriter op CoreComp
                 , PipeReader ip CoreComp, PipeWriter op CoreComp)
               => CoreId -> Size -> ip a -> op a -> CoreComp ()
            f = g (+16)
        onCore 0 (f 0 bulkSize ph_0 p0_h)
        runBenchmark ioChunkSize bulkSize ph_0 p0_h


pattern1 :: Benchmark
pattern1 ioChunkSize bulkSize = do
    ph_0 :: HostToCorePipe Int32 <- allocHostPipe 0 ioChunkSize
    p0_1 <- allocCorePipe 0 1 bulkSize
    p1_h <- allocHostPipe   1 ioChunkSize
    onHost $ do
        initPipe ph_0
        initPipe p0_1
        initPipe p1_h
        let f :: ( Num a, SmallType a
                 , BulkPipeReader ip CoreComp, BulkPipeWriter op CoreComp
                 , PipeReader ip CoreComp, PipeWriter op CoreComp)
               => CoreId -> Size -> ip a -> op a -> CoreComp ()
            f = g (+8)
        onCore 0 ((h (+8)) 0 bulkSize ph_0 p0_1)
        onCore 1 (f 1 bulkSize p0_1 p1_h)
        runBenchmark ioChunkSize bulkSize ph_0 p1_h


pattern2 :: Benchmark
pattern2 ioChunkSize bulkSize = do
    ph_0 :: HostToCorePipe Int32 <- allocHostPipe 0 ioChunkSize
    p0_4   <- allocCorePipe  0  4 bulkSize
    p4_8   <- allocCorePipe  4  8 bulkSize
    p8_12  <- allocCorePipe  8 12 bulkSize
    p12_13 <- allocCorePipe 12 13 bulkSize
    p13_14 <- allocCorePipe 13 14 bulkSize
    p14_15 <- allocCorePipe 14 15 bulkSize
    p15_11 <- allocCorePipe 15 11 bulkSize
    p11_7  <- allocCorePipe 11  7 bulkSize
    p7_3   <- allocCorePipe  7  3 bulkSize
    p3_2   <- allocCorePipe  3  2 bulkSize
    p2_1   <- allocCorePipe  2  1 bulkSize
    p1_5   <- allocCorePipe  1  5 bulkSize
    p5_9   <- allocCorePipe  5  9 bulkSize
    p9_10  <- allocCorePipe  9 10 bulkSize
    p10_6  <- allocCorePipe 10  6 bulkSize
    p6_h   <- allocHostPipe     6 ioChunkSize
    onHost $ do
        initPipe ph_0
        initPipe p6_h
        mapM initPipe
            [        p2_1  , p3_2  , p7_3
            , p0_4 , p1_5  , p10_6 , p11_7
            , p4_8 , p5_9  , p9_10 , p15_11
            , p8_12, p12_13, p13_14, p14_15 ]
        let f :: ( Num a, SmallType a
                 , BulkPipeReader ip CoreComp, BulkPipeWriter op CoreComp
                 , PipeReader ip CoreComp, PipeWriter op CoreComp)
               => CoreId -> Size -> ip a -> op a -> CoreComp ()
            f = g (+1)
        onCore  0 ((h (+1))  0 bulkSize ph_0   p0_4)
        onCore  4 (f  4 bulkSize p0_4   p4_8)
        onCore  8 (f  8 bulkSize p4_8   p8_12)
        onCore 12 (f 12 bulkSize p8_12  p12_13)
        onCore 13 (f 13 bulkSize p12_13 p13_14)
        onCore 14 (f 14 bulkSize p13_14 p14_15)
        onCore 15 (f 15 bulkSize p14_15 p15_11)
        onCore 11 (f 11 bulkSize p15_11 p11_7)
        onCore  7 (f  7 bulkSize p11_7  p7_3)
        onCore  3 (f  3 bulkSize p7_3   p3_2)
        onCore  2 (f  2 bulkSize p3_2   p2_1)
        onCore  1 (f  1 bulkSize p2_1   p1_5)
        onCore  5 (f  5 bulkSize p1_5   p5_9)
        onCore  9 (f  9 bulkSize p5_9   p9_10)
        onCore 10 (f 10 bulkSize p9_10  p10_6)
        onCore  6 (f  6 bulkSize p10_6 p6_h)
        runBenchmark ioChunkSize bulkSize ph_0 p6_h


h :: ( Num a, SmallType a
     , BulkPipeReader ip CoreComp, BulkPipeWriter op CoreComp
     , PipeReader ip CoreComp, PipeWriter op CoreComp)
   => (Data a -> Data a) -> CoreId -> Size -> ip a -> op a -> CoreComp ()
h f coreId = mapBulk f

mapBulk :: (SmallType a, BulkPipeReader ip CoreComp, BulkPipeWriter op CoreComp)
        => (Data a -> Data a) -> Size -> ip a -> op a -> CoreComp ()
mapBulk f bulkSize input output = forever $ do
    tmp <- newArr $ value bulkSize
    pullPipe input (0, value $ bulkSize - 1) tmp
    for (0, 1, Excl $ value bulkSize) $ \i -> do
        elem :: Data a <- getArr i tmp
        setArr i (f elem) tmp
    pushPipe output (0, value $ bulkSize - 1) tmp

g :: ( Num a, SmallType a
     , BulkPipeReader ip CoreComp, BulkPipeWriter op CoreComp
     , PipeReader ip CoreComp, PipeWriter op CoreComp)
   => (Data a -> Data a) -> CoreId -> Size -> ip a -> op a -> CoreComp ()
g f coreId = mapBulk f -- mapInPlace f

mapInPlace :: (SmallType a, PipeReader ip CoreComp, BulkPipeWriter op CoreComp)
        => (Data a -> Data a) -> Size -> ip a -> op a -> CoreComp ()
mapInPlace f bulkSize input output = forever $ do
    tmp <- newArr $ value bulkSize
    for (0, 1, Excl $ value bulkSize) $ \i -> do
        elem :: Data a <- readPipe input
        setArr i (f elem) tmp
    pushPipe output (0, value $ bulkSize - 1) tmp

map :: (SmallType a, PipeReader ip CoreComp, PipeWriter op CoreComp)
        => (Data a -> Data a) -> Size -> ip a -> op a -> CoreComp ()
map f _ input output = forever $ do
    elem <- readPipe input
    writePipe (f elem) output


runBenchmark :: SmallType a => Size -> Size -> HostToCorePipe a -> CoreToHostPipe a -> Host ()
runBenchmark ioChunkSize bulkSize fromHost toHost = do
        lift $ addInclude "<zbenchmark.h>"
        lift $ callProc "setup_trace" []
        lift $ callProc "open_files" []
        input  <- newArr $ value ioChunkSize
        output <- newArr $ value ioChunkSize
        let read = lift $ externFun "read_chunk" [ arrArg input, valArg $ value ioChunkSize ]
        while (read) $ do
            lift $ callProc "start_iteration" []
            pushPipe fromHost (0, value $ ioChunkSize - 1) input
            pullPipe toHost   (0, value $ ioChunkSize - 1) output
            lift $ callProc "end_iteration" []
            lift $ callProc "write_chunk" [ arrArg output, valArg $ value ioChunkSize ]
        lift $ callProc "close_files" []
        lift $ callProc "print_time" []


------------------------------------------------------------

main = do
    let sizes = [(64, 64), (512, 512), (1024, 1024), (8192, 1024)]
    forM sizes $ \(iocs, bs) -> do
        let patterns = Prelude.zip [ pattern0, pattern1, pattern2 ] [0..]
        forM patterns $ \(p, i) -> do
            let test = p iocs bs
                dirName = "benchmarking/"
                     Prelude.++ show i
                     Prelude.++ "_"
                     Prelude.++ show iocs
                     Prelude.++ "_"
                     Prelude.++ show bs
            icompileAll `onParallella` test
            let modules = compileAll `onParallella` test
            createDirectory  dirName
            forM_ modules $ \(name, contents) -> do
                let name' = if name Prelude.== "main" then "host" else name
                writeFile (dirName
                  Prelude.++ "/"
                  Prelude.++ name'
                  Prelude.++ ".c") contents
