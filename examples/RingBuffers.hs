module RingBuffers where

import qualified Prelude

import Feldspar.Multicore


newtype SpmRef a = SpmRef { unSpmRef :: Arr a }

allocSpmRef :: SmallType a => CoreId -> Multicore (SpmRef a)
allocSpmRef coreId = SpmRef <$> allocArr coreId 1

fetchSpmRef :: SmallType a => SpmRef a -> Data a -> Host ()
fetchSpmRef spmRef value = do
    tmp <- newArr 1
    setArr 0 value tmp
    writeArr (unSpmRef spmRef) (0,0) tmp

flushSpmRef :: SmallType a => SpmRef a -> Host (Data a)
flushSpmRef spmRef = do
    tmp <- newArr 1
    readArr (unSpmRef spmRef) (0,0) tmp
    getArr 0 tmp

getSpmRef :: SmallType a => SpmRef a -> CoreComp (Data a)
getSpmRef = getArr 0 . unSpmRef

setSpmRef :: SmallType a => Data a -> SpmRef a-> CoreComp ()
setSpmRef value = setArr 0 value . unSpmRef


data Buffer a = Buffer
    { readPtr  :: SpmRef Index
    , writePtr :: SpmRef Index
    , elements :: Arr a
    , maxElems :: Data Length
    }

allocBuff :: SmallType a => CoreId -> Size -> Multicore (Buffer a)
allocBuff coreId size = do
    rptr  <- allocSpmRef coreId
    wptr  <- allocSpmRef coreId
    elems <- allocArr coreId (size + 1)
    return $ Buffer rptr wptr elems (value size + 1)

initBuff :: SmallType a => Buffer a -> Host ()
initBuff (Buffer rptr wptr _ _) = do
    fetchSpmRef rptr 0
    fetchSpmRef wptr 0

fetchBuff :: SmallType a => Buffer a -> IndexRange -> Arr a -> Host ()
fetchBuff (Buffer rptr wptr elems size) (lower, upper) src = do
    let total = upper - lower + 1
    written <- initRef (value 0 :: Data Length)
    -- until all data is written
    while ((<total) <$> getRef written) $ do
        -- wait for empty space in the buffer
        while (do
            rx <- flushSpmRef rptr
            wx <- flushSpmRef wptr
            return $ (wx + 1) `rem` size == rx) $ return ()
        -- calculate items left
        done <- getRef written
        let left = total - done
        -- how many items could be writen in this round
        rx <- flushSpmRef rptr
        wx <- flushSpmRef wptr
        let available = ((size + wx - rx) `rem` size)
            empty = size - available - 1  -- one slot is reserved in this setup
            toWrite = min left empty
            start = lower + done
        iff (wx + toWrite <= size)
            (writeArrAt wx elems (start, start + toWrite - 1) src)
            (do let toEnd = size - wx
                writeArrAt wx elems (start, start + toEnd - 1) src
                let start' = start + toEnd
                writeArrAt 0 elems (start', start' + (toWrite - toEnd) - 1) src)
        fetchSpmRef wptr ((wx + toWrite) `rem` size)
        setRef written (done + toWrite)

flushBuff :: SmallType a => Buffer a -> IndexRange -> Arr a -> Host ()
flushBuff (Buffer rptr wptr elems size) (lower, upper) dst = do
    let total = upper - lower + 1
    read <- initRef (value 0 :: Data Length)
    -- until all data is read
    while ((<total) <$> getRef read) $ do
        -- wait for items in the buffer
        while (do
            rx <- flushSpmRef rptr
            wx <- flushSpmRef wptr
            return $ rx == wx) $ return ()
        -- calculate items left
        done <- getRef read
        let left = total - done
        -- how many items could it read in this round
        rx <- flushSpmRef rptr
        wx <- flushSpmRef wptr
        let available = (size + wx -rx) `rem` size
        let toRead = min left available
            start = lower + done
        iff (rx + toRead <= size)
            (readArrAt rx elems (start, start + toRead - 1) dst)
            (do let toEnd = size - rx
                readArrAt rx elems (start, start + toEnd - 1) dst
                let start' = start + toEnd
                readArrAt 0 elems (start', start' + toRead - (size - rx) - 1) dst)
        fetchSpmRef rptr ((rx + toRead) `rem` size)
        setRef read (done + toRead)

writeBuff :: SmallType a => Data a -> Buffer a -> CoreComp ()
writeBuff elem (Buffer rptr wptr elems size) = do
    while (do
        rx <- getSpmRef rptr
        wx <- getSpmRef wptr
        return $ (wx + 1) `rem` size == rx) $ return ()
    wx <- getSpmRef wptr
    setArr wx elem elems
    setSpmRef ((wx + 1) `rem` size) wptr

readBuff :: SmallType a => Buffer a -> CoreComp (Data a)
readBuff (Buffer rptr wptr elems size) = do
    while (do
        rx <- getSpmRef rptr
        wx <- getSpmRef wptr
        return $ rx == wx) $ return ()
    rx <- getSpmRef rptr
    elem <- getArr rx elems
    setSpmRef ((rx + 1) `rem` size) rptr
    return elem


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
f input output = while (return $ true) $ do
    elem <- readBuff input
    writeBuff (elem + 1) output

g :: Buffer Int32 -> Buffer Int32 -> CoreComp ()
g input output = while (return $ true) $ do
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
