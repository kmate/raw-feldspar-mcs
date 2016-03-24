module Feldspar.Multicore.Pipe where

import qualified Prelude
import Control.Monad.Trans

import Feldspar
import Feldspar.Multicore.Frontend
import Feldspar.Multicore.Reference
import Feldspar.Multicore.Representation
import Feldspar.Option


--------------------------------------------------------------------------------
-- Frontend
--------------------------------------------------------------------------------

class PipeInit p
  where
    initPipe :: SmallType a => p a -> Host ()


class MonadComp m => PipeReader p m
  where
    readPipeA :: SmallType a => p a -> OptionT m (Data a)
    readPipe  :: SmallType a => p a -> m (Data a)
    readPipe pipe = do
            value <- newRef
            noValue <- initRef true
            while (getRef noValue) $ do
                caseOptionT (readPipeA pipe)
                    (const $ return ())
                    (setRef value >=> (const $ setRef noValue false))
            getRef value

class MonadComp m => PipeWriter p m
  where
    writePipeA :: SmallType a => Data a -> p a -> m (Data Bool)
    writePipe  :: SmallType a => Data a -> p a -> m ()
    writePipe elem pipe = while (not <$> writePipeA elem pipe) $ return ()


class MonadComp m => BulkPipeReader p m
  where
    pullPipe  :: SmallType a => p a -> IndexRange -> Arr a -> m ()

class MonadComp m => BulkPipeWriter p m
  where
    pushPipe   :: SmallType a => p a -> IndexRange -> Arr a -> m ()


--------------------------------------------------------------------------------
-- Host-to-core and core-to-host pipes
--------------------------------------------------------------------------------

data FromHost
data ToHost

data HostPipe dir a = HostPipe
    { hpReadPtr  :: LocalRef Index -- both pointers in core memory
    , hpWritePtr :: LocalRef Index
    , hpElements :: SharedArr a    -- elements in external memory
    , hpMaxElems :: Data Length
    }

type HostToCorePipe = HostPipe FromHost
type CoreToHostPipe = HostPipe ToHost


allocHostPipe :: SmallType a => CoreId -> Size -> Multicore (HostPipe dir a)
allocHostPipe coreId size = do
    rptr  <- allocRef coreId
    wptr  <- allocRef coreId
    elems <- allocSArr (size + 1)
    return $ HostPipe rptr wptr elems (value size + 1)


instance PipeInit (HostPipe dir)
  where
    initPipe (HostPipe rptr wptr _ _) = do
        setLocalRef rptr 0
        setLocalRef wptr 0


instance BulkPipeReader (HostPipe ToHost) Host
  where
    pullPipe (HostPipe rptr wptr elems size) (lower, upper) dst = do
        let total = upper - lower + 1
        read <- initRef (value 0 :: Data Length)
        -- until all data is read
        while ((<total) <$> getRef read) $ do
            -- wait for items in the buffer
            while (do
                rx <- getLocalRef rptr
                wx <- getLocalRef wptr
                return $ rx == wx) $ return ()
            -- calculate items left
            done <- getRef read
            let left = total - done
            -- how many items could it read in this round
            rx <- getLocalRef rptr
            wx <- getLocalRef wptr
            let available = (size + wx -rx) `rem` size
            let toRead = min left available
                start = lower + done
            iff (rx + toRead <= size)
                (readArrAt rx elems (start, start + toRead - 1) dst)
                (do let toEnd = size - rx
                    readArrAt rx elems (start, start + toEnd - 1) dst
                    let start' = start + toEnd
                    readArrAt 0 elems (start', start' + toRead - (size - rx) - 1) dst)
            setLocalRef rptr ((rx + toRead) `rem` size)
            setRef read (done + toRead)

instance BulkPipeWriter (HostPipe FromHost) Host
  where
    pushPipe (HostPipe rptr wptr elems size) (lower, upper) src = do
        let total = upper - lower + 1
        written <- initRef (value 0 :: Data Length)
        -- until all data is written
        while ((<total) <$> getRef written) $ do
            -- wait for empty space in the buffer
            while (do
                rx <- getLocalRef rptr
                wx <- getLocalRef wptr
                return $ (wx + 1) `rem` size == rx) $ return ()
            -- calculate items left
            done <- getRef written
            let left = total - done
            -- how many items could be writen in this round
            rx <- getLocalRef rptr
            wx <- getLocalRef wptr
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
            setLocalRef wptr ((wx + toWrite) `rem` size)
            setRef written (done + toWrite)


instance PipeReader (HostPipe FromHost) CoreComp
  where
    readPipeA (HostPipe rptr wptr elems size) = do
        rx <- lift $ getLocalRef rptr
        wx <- lift $ getLocalRef wptr
        guarded "no data" (rx /= wx) ()
        lift $ do
            rx <- getLocalRef rptr
            tmp <- newArr 1
            readArrAt rx elems (0,0) tmp
            elem <- getArr 0 tmp
            setLocalRef rptr ((rx + 1) `rem` size)
            return elem

instance PipeWriter (HostPipe ToHost) CoreComp
  where
    writePipeA elem (HostPipe rptr wptr elems size) = do
        rx <- getLocalRef rptr
        wx <- getLocalRef wptr
        done <- initRef false
        iff ((wx + 1) `rem` size == rx)
            (setRef done false)
            (do
                wx <- getLocalRef wptr
                tmp <- newArr 1
                setArr 0 elem tmp
                writeArrAt wx elems (0,0) tmp
                setLocalRef wptr ((wx + 1) `rem` size)
                setRef done true)
        getRef done


--------------------------------------------------------------------------------
-- Core-to-core pipes
--------------------------------------------------------------------------------

data CorePipe a = CorePipe
    { cpReadPtr   :: LocalRef Index -- original read pointer in reader core
    , cpWritePtr  :: LocalRef Index -- original write pointer in writer core
    , cpReadPtrS  :: LocalRef Index -- shadow read pointer in writer core
    , cpWritePtrS :: LocalRef Index -- shadow write pointer in reader core
    , cpElements  :: LocalArr a     -- elements in reader core
    , cpMaxElems  :: Data Length
    }


instance PipeInit CorePipe
  where
    initPipe (CorePipe rptr wptr rptrS wptrS _ _) = do
        setLocalRef rptr  0
        setLocalRef wptr  0
        setLocalRef rptrS 0
        setLocalRef wptrS 0


allocCorePipe :: SmallType a => CoreId -> CoreId -> Size -> Multicore (CorePipe a)
allocCorePipe writer reader size = do
    rptr  <- allocRef reader
    wptr  <- allocRef writer
    rptrS <- allocRef writer
    wptrS <- allocRef reader
    elems <- allocLArr reader (size + 1)
    return $ CorePipe rptr wptr rptrS wptrS elems (value size + 1)


instance PipeReader CorePipe CoreComp
  where
    readPipeA (CorePipe rptr wptr rptrS wptrS elems size) = do
        rx <- lift $ getLocalRef rptr
        wx <- lift $ getLocalRef wptrS -- read local shadow copy of write pointer
        guarded "no data" (rx /= wx) ()
        lift $ do
            rx <- getLocalRef rptr
            elem <- getArr rx -< elems
            let rx' = (rx + 1) `rem` size
            setLocalRef rptr  rx'
            setLocalRef rptrS rx' -- write remote shadow copy of read pointer
            return elem

instance PipeWriter CorePipe CoreComp
  where
    writePipeA elem (CorePipe rptr wptr rptrS wptrS elems size) = do
        rx <- getLocalRef rptrS -- read local shadow copy of write pointer
        wx <- getLocalRef wptr
        done <- initRef false
        let wx' = (wx + 1) `rem` size
        iff (wx' == rx)
            (setRef done false)
            (do
                wx <- getLocalRef wptr
                setArr wx elem -< elems
                setLocalRef wptr  wx'
                setLocalRef wptrS wx' -- write remote shadow copy of write pointer
                setRef done true)
        getRef done


--------------------------------------------------------------------------------
-- Old interface
--------------------------------------------------------------------------------

data Pipe a = Pipe
    { readPtr  :: LocalRef Index
    , writePtr :: LocalRef Index
    , elements :: LocalArr a
    , maxElems :: Data Length
    }


allocPipe :: SmallType a => CoreId -> Size -> Multicore (Pipe a)
allocPipe coreId size = do
    rptr  <- allocRef coreId
    wptr  <- allocRef coreId
    elems <- allocLArr coreId (size + 1)
    return $ Pipe rptr wptr elems (value size + 1)


instance PipeInit Pipe
  where
    initPipe (Pipe rptr wptr _ _) = do
        setLocalRef rptr 0
        setLocalRef wptr 0


instance PipeReader Pipe CoreComp
  where
    readPipeA (Pipe rptr wptr elems size) = do
        rx <- lift $ getLocalRef rptr
        wx <- lift $ getLocalRef wptr
        guarded "no data" (rx /= wx) ()
        lift $ do
            rx <- getLocalRef rptr
            elem <- getArr rx -< elems
            setLocalRef rptr ((rx + 1) `rem` size)
            return elem

instance PipeWriter Pipe CoreComp
  where
    writePipeA elem (Pipe rptr wptr elems size) = do
        rx <- getLocalRef rptr
        wx <- getLocalRef wptr
        done <- initRef false
        iff ((wx + 1) `rem` size == rx)
            (setRef done false)
            (do
                wx <- getLocalRef wptr
                setArr wx elem -< elems
                setLocalRef wptr ((wx + 1) `rem` size)
                setRef done true)
        getRef done


instance BulkPipeReader Pipe Host
  where
    pullPipe (Pipe rptr wptr elems size) (lower, upper) dst = do
        let total = upper - lower + 1
        read <- initRef (value 0 :: Data Length)
        -- until all data is read
        while ((<total) <$> getRef read) $ do
            -- wait for items in the buffer
            while (do
                rx <- getLocalRef rptr
                wx <- getLocalRef wptr
                return $ rx == wx) $ return ()
            -- calculate items left
            done <- getRef read
            let left = total - done
            -- how many items could it read in this round
            rx <- getLocalRef rptr
            wx <- getLocalRef wptr
            let available = (size + wx -rx) `rem` size
            let toRead = min left available
                start = lower + done
            iff (rx + toRead <= size)
                (readArrAt rx elems (start, start + toRead - 1) dst)
                (do let toEnd = size - rx
                    readArrAt rx elems (start, start + toEnd - 1) dst
                    let start' = start + toEnd
                    readArrAt 0 elems (start', start' + toRead - (size - rx) - 1) dst)
            setLocalRef rptr ((rx + toRead) `rem` size)
            setRef read (done + toRead)

instance BulkPipeWriter Pipe Host
  where
    pushPipe (Pipe rptr wptr elems size) (lower, upper) src = do
        let total = upper - lower + 1
        written <- initRef (value 0 :: Data Length)
        -- until all data is written
        while ((<total) <$> getRef written) $ do
            -- wait for empty space in the buffer
            while (do
                rx <- getLocalRef rptr
                wx <- getLocalRef wptr
                return $ (wx + 1) `rem` size == rx) $ return ()
            -- calculate items left
            done <- getRef written
            let left = total - done
            -- how many items could be writen in this round
            rx <- getLocalRef rptr
            wx <- getLocalRef wptr
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
            setLocalRef wptr ((wx + toWrite) `rem` size)
            setRef written (done + toWrite)
