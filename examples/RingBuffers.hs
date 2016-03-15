module RingBuffers where

import qualified Prelude

import Feldspar.Multicore


newtype SpmRef a = SpmRef { unSpmRef :: Arr a }

allocSpmRef :: SmallType a => CoreId -> Multicore (SpmRef a)
allocSpmRef coreId = SpmRef <$> alloc coreId 1

fetchSpmRef :: SmallType a => SpmRef a -> Data a -> Host ()
fetchSpmRef spmRef value = do
    tmp <- newArr 1
    setArr 0 value tmp
    fetch (unSpmRef spmRef) (0,0) tmp

flushSpmRef :: SmallType a => SpmRef a -> Host (Data a)
flushSpmRef spmRef = do
    tmp <- newArr 1
    flush (unSpmRef spmRef) (0,0) tmp
    getArr 0 tmp

getSpmRef :: SmallType a => SpmRef a -> Comp (Data a)
getSpmRef = getArr 0 . unSpmRef

setSpmRef :: SmallType a => Data a -> SpmRef a-> Comp ()
setSpmRef value = setArr 0 value . unSpmRef


data Buffer a = Buffer
    { readPtr  :: SpmRef Index
    , writePtr :: SpmRef Index
    , numElems :: SpmRef Length
    , elements :: Arr a
    , maxElems :: Data Length
    }

allocBuff :: SmallType a => CoreId -> Size -> Multicore (Buffer a)
allocBuff coreId size = do
    rptr  <- allocSpmRef coreId
    wptr  <- allocSpmRef coreId
    nels  <- allocSpmRef coreId
    elems <- alloc coreId size
    return $ Buffer rptr wptr nels elems (value size)

initBuff :: SmallType a => Buffer a -> Host ()
initBuff (Buffer rptr wptr nels _ _) = do
    fetchSpmRef rptr 0
    fetchSpmRef wptr 0
    fetchSpmRef nels 0

fetchBuff :: SmallType a => Buffer a -> IndexRange -> Arr a -> Host ()
fetchBuff (Buffer rptr wptr nels elems size) (lower, upper) src = do
    let total = upper - lower + 1
    while ((<0) <$> flushSpmRef nels) $ return ()
    fetch elems (lower, upper) src
    fetchSpmRef nels total
{-
    let total = upper - lower + 1
    written <- initRef (value 0 :: Data Length)
    while ((<total) <$> getRef written) $ do
        -- wait for empty space in the buffer
        while ((==size) <$> flushSpmRef nels) $ return ()
        -- calculate items left
        done <- getRef written
        let left = total - done
        -- how many items could it write in this round
        count <- flushSpmRef nels
        let toWrite = min left (left - (size - count))
        let start = lower + done
        wx <- flushSpmRef wptr
        iff (wx + toWrite <= size)
            (fetchTo wx elems (start, start + toWrite - 1) src)
            (do let toEnd = size - wx
                fetchTo wx elems (start, start + toEnd - 1) src
                let start' = start + toEnd
                fetchTo 0 elems (start', start' + toWrite - (size - wx) - 1) src)
        fetchSpmRef wptr ((wx + toWrite) `rem` size)
-- should be mutexed together with the getter of count above
        fetchSpmRef nels (count + toWrite)
-}

flushBuff :: SmallType a => Buffer a -> IndexRange -> Arr a -> Host ()
flushBuff (Buffer rptr wptr nels elems size) (lower, upper) dst = do
    let total = upper - lower + 1
    while ((==total) <$> flushSpmRef nels) $ return ()
    flush elems (lower, upper) dst
    fetchSpmRef nels 0
{-
    let total = upper - lower + 1
    read <- initRef (value 0 :: Data Length)
    while ((<total) <$> getRef read) $ do
        -- wait for items in the buffer
        while ((<=0) <$> flushSpmRef nels) $ return ()
        -- calculate items left
        done <- getRef read
        let left = total - done
        -- how many items could it read in this round
        count <- flushSpmRef nels
        let toRead = min left (left - (size - count))
        let start = lower + done
        rx <- flushSpmRef rptr
        iff (rx + toRead <= size)
            (flushFrom rx elems (start, start + toRead - 1) src)
            (do let toEnd = size - rx
                flushFrom rx elems (start, start + toEnd - 1) src
                let start' = start + toEnd
                fetchTo 0 elems (start', start' + toRead - (size - rx) - 1) src)
        fetchSpmRef rptr ((rx + toRead) `rem` size)
-- should be mutexed together with the getter of count above
        fetchSpmRef nels (count - toRead)
-}

writeBuff :: SmallType a => Data a -> Buffer a -> Comp ()
writeBuff elem (Buffer rptr wptr nels elems size) = do
    while ((==size) <$> getSpmRef nels) $ return ()
    wx <- getSpmRef wptr
    setSpmRef ((wx + 1) `rem` size) wptr
    setArr wx elem elems
-- mutex start
    count :: Data Length <- getSpmRef nels
    setSpmRef (count + 1) nels
-- mutex end

readBuff :: SmallType a => Buffer a -> Comp (Data a)
readBuff (Buffer rptr wptr nels elems size) = do
    while ((<=0) <$> getSpmRef nels) $ return ()
    rx <- getSpmRef rptr
    setSpmRef ((rx + 1) `rem` size) rptr
    elem <- getArr rx elems
-- mutex start
    count :: Data Length <- getSpmRef nels
    setSpmRef (count - 1) nels
-- mutex end
    return elem


n :: Word32
n = 4

ringBuffers :: Multicore ()
ringBuffers = do
    b0 <- allocBuff 0 4
    b1 <- allocBuff 1 4
    b2 <- allocBuff 2 4
    onHost $ do
        initBuff b0
        initBuff b1
        initBuff b2
        onCore 0 (f b0 b1)
        onCore 1 (g b1 b2)

        while (return $ true) $ do
            input :: Arr Int32 <- newArr $ value n
            for (0, 1, Excl $ value n) $ \i -> do
                item :: Data Int32 <- lift $ fget stdin
                setArr i item input

            fetchBuff b0 (0, value $ n - 1) input
            output <- newArr $ value n
            flushBuff b2 (0, value $ n - 1) output

            for (0, 1, Excl $ value n) $ \i -> do
                item :: Data Int32 <- getArr i output
                lift $ printf "> %d\n" item


f :: Buffer Int32 -> Buffer Int32 -> Comp ()
f input output = while (return $ true) $ do
    elem <- readBuff input
    writeBuff (elem + 1) output

g :: Buffer Int32 -> Buffer Int32 -> Comp ()
g input output = while (return $ true) $ do
    elem <- readBuff input
    writeBuff (elem * 2) output


------------------------------------------------------------

testAll = do
    icompileAll `onParallella` ringBuffers
    let modules = compileAll `onParallella` ringBuffers
    forM_ modules $ \(name, contents) -> do
        let name' = if name Prelude.== "main" then "host" else name
        writeFile (name' Prelude.++ ".c") contents
