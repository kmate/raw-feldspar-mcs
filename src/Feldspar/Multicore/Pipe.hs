module Feldspar.Multicore.Pipe where

import qualified Prelude

import Feldspar
import Feldspar.Multicore.Frontend
import Feldspar.Multicore.Reference
import Feldspar.Multicore.Representation


data Buffer a = Buffer
    { readPtr  :: LocalRef Index
    , writePtr :: LocalRef Index
    , elements :: LocalArr a
    , maxElems :: Data Length
    }

allocBuff :: SmallType a => CoreId -> Size -> Multicore (Buffer a)
allocBuff coreId size = do
    rptr  <- allocRef coreId
    wptr  <- allocRef coreId
    elems <- allocArr coreId (size + 1)
    return $ Buffer rptr wptr elems (value size + 1)

initBuff :: SmallType a => Buffer a -> Host ()
initBuff (Buffer rptr wptr _ _) = do
    writeRef rptr 0
    writeRef wptr 0

fetchBuff :: SmallType a => Buffer a -> IndexRange -> Arr a -> Host ()
fetchBuff (Buffer rptr wptr elems size) (lower, upper) src = do
    let total = upper - lower + 1
    written <- initRef (value 0 :: Data Length)
    -- until all data is written
    while ((<total) <$> getRef written) $ do
        -- wait for empty space in the buffer
        while (do
            rx <- readRef rptr
            wx <- readRef wptr
            return $ (wx + 1) `rem` size == rx) $ return ()
        -- calculate items left
        done <- getRef written
        let left = total - done
        -- how many items could be writen in this round
        rx <- readRef rptr
        wx <- readRef wptr
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
        writeRef wptr ((wx + toWrite) `rem` size)
        setRef written (done + toWrite)

flushBuff :: SmallType a => Buffer a -> IndexRange -> Arr a -> Host ()
flushBuff (Buffer rptr wptr elems size) (lower, upper) dst = do
    let total = upper - lower + 1
    read <- initRef (value 0 :: Data Length)
    -- until all data is read
    while ((<total) <$> getRef read) $ do
        -- wait for items in the buffer
        while (do
            rx <- readRef rptr
            wx <- readRef wptr
            return $ rx == wx) $ return ()
        -- calculate items left
        done <- getRef read
        let left = total - done
        -- how many items could it read in this round
        rx <- readRef rptr
        wx <- readRef wptr
        let available = (size + wx -rx) `rem` size
        let toRead = min left available
            start = lower + done
        iff (rx + toRead <= size)
            (readArrAt rx elems (start, start + toRead - 1) dst)
            (do let toEnd = size - rx
                readArrAt rx elems (start, start + toEnd - 1) dst
                let start' = start + toEnd
                readArrAt 0 elems (start', start' + toRead - (size - rx) - 1) dst)
        writeRef rptr ((rx + toRead) `rem` size)
        setRef read (done + toRead)

writeBuff :: SmallType a => Data a -> Buffer a -> CoreComp ()
writeBuff elem (Buffer rptr wptr elems size) = do
    while (do
        rx <- getLocalRef rptr
        wx <- getLocalRef wptr
        return $ (wx + 1) `rem` size == rx) $ return ()
    wx <- getLocalRef wptr
    setArr wx elem -< elems
    setLocalRef ((wx + 1) `rem` size) wptr

readBuff :: SmallType a => Buffer a -> CoreComp (Data a)
readBuff (Buffer rptr wptr elems size) = do
    while (do
        rx <- getLocalRef rptr
        wx <- getLocalRef wptr
        return $ rx == wx) $ return ()
    rx <- getLocalRef rptr
    elem <- getArr rx -< elems
    setLocalRef ((rx + 1) `rem` size) rptr
    return elem
