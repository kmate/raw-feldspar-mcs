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

class (Pipe p, LocalRefAccess m) => PipeReader p m
  where
    getElement :: SmallType a => p a -> Data Index -> m (Data a)

readPipeA :: (Pipe p, PipeReader p m, SmallType a)
          => p a -> OptionT m (Data a)
readPipeA pipe = do
    let size = getSize pipe
    rx <- lift $ getCReadPtr  pipe
    wx <- lift $ getCWritePtr pipe
    guarded "no data" (rx /= wx) ()
    lift $ do
        rx <- getCReadPtr pipe
        elem <- getElement pipe rx
        setReadPtr pipe ((rx + 1) `rem` size)
        return elem

readPipe  :: (Pipe p, PipeReader p m, SmallType a)
          => p a -> m (Data a)
readPipe pipe = do
        value <- newRef
        noValue <- initRef true
        while (getRef noValue) $ do
            caseOptionT (readPipeA pipe)
                (const $ return ())
                (setRef value >=> (const $ setRef noValue false))
        getRef value


class (Pipe p, LocalRefAccess m) => PipeWriter p m
  where
    setElement :: SmallType a => p a -> Data Index -> Data a -> m ()

writePipeA :: (Pipe p, PipeWriter p m, SmallType a)
           => Data a -> p a -> m (Data Bool)
writePipeA elem pipe = do
    let size = getSize pipe
    rx <- getPReadPtr  pipe
    wx <- getPWritePtr pipe
    done <- initRef false
    let wx' = (wx + 1) `rem` size
    iff (wx' == rx)
        (setRef done false)
        (do
            wx <- getPWritePtr pipe
            setElement pipe wx elem
            setWritePtr pipe wx'
            setRef done true)
    getRef done

writePipe  :: (Pipe p, PipeWriter p m, SmallType a) => Data a -> p a -> m ()
writePipe elem pipe = while (not <$> writePipeA elem pipe) $ return ()


class (Pipe p, LocalRefAccess m) => BulkPipeReader p m
  where
    getElements :: SmallType a => p a -> Data Index -> IndexRange -> Arr a -> m ()

pullPipeA :: (Pipe p, BulkPipeReader p m, SmallType a)
          => p a -> IndexRange -> Arr a -> m (Data Index)
pullPipeA pipe  (lower, upper) dst = do
    -- wait for items in the buffer
    while (do
        rx <- getCReadPtr pipe
        wx <- getCWritePtr pipe
        return $ rx == wx) $ return ()
    -- calculate items left
    let left = upper - lower + 1
    -- how many items could it read in this round
    rx <- getCReadPtr pipe
    wx <- getCWritePtr pipe
    let size = getSize pipe
        available = (size + wx -rx) `rem` size
        toRead = min left available
        start = lower
    iff (rx + toRead <= size)
        (getElements pipe rx (start, start + toRead - 1) dst)
        (do let toEnd = size - rx
            getElements pipe rx (start, start + toEnd - 1) dst
            let start' = start + toEnd
            getElements pipe 0 (start', start' + toRead - (size - rx) - 1) dst)
    setReadPtr pipe ((rx + toRead) `rem` size)
    return toRead

pullPipe  :: (Pipe p, BulkPipeReader p m, SmallType a)
          => p a -> IndexRange -> Arr a -> m ()
pullPipe pipe (lower, upper) dst = do
    let total = upper - lower + 1
    read <- initRef (value 0 :: Data Length)
    -- until all data is read
    while ((<total) <$> getRef read) $ do
        done <- getRef read
        toRead <- pullPipeA pipe (lower + done, upper) dst
        setRef read (done + toRead)


class (Pipe p, LocalRefAccess m) => BulkPipeWriter p m
  where
    setElements :: SmallType a => p a -> Data Index -> IndexRange -> Arr a -> m ()

pushPipeA :: (Pipe p, BulkPipeWriter p m, SmallType a)
          => p a -> IndexRange -> Arr a -> m (Data Index)
pushPipeA pipe (lower, upper) src = do
    let size = getSize pipe
    -- wait for empty space in the buffer
    while (do
        rx <- getPReadPtr  pipe
        wx <- getPWritePtr pipe
        return $ (wx + 1) `rem` size == rx) $ return ()
    -- calculate items left
    let left = upper - lower + 1
    -- how many items could be writen in this round
    rx <- getPReadPtr  pipe
    wx <- getPWritePtr pipe
    let available = ((size + wx - rx) `rem` size)
        empty = size - available - 1  -- one slot is reserved in this setup
        toWrite = min left empty
        start = lower
    iff (wx + toWrite <= size)
        (setElements pipe wx (start, start + toWrite - 1) src)
        (do let toEnd = size - wx
            setElements pipe wx (start, start + toEnd - 1) src
            let start' = start + toEnd
            setElements pipe 0 (start', start' + (toWrite - toEnd) - 1) src)
    setWritePtr pipe ((wx + toWrite) `rem` size)
    return toWrite

pushPipe  :: (Pipe p, BulkPipeWriter p m, SmallType a)
          => p a -> IndexRange -> Arr a -> m ()
pushPipe pipe (lower, upper) src = do
    let total = upper - lower + 1
    written <- initRef (value 0 :: Data Length)
    -- until all data is written
    while ((<total) <$> getRef written) $ do
        done <- getRef written
        toWrite <- pushPipeA pipe (lower + done, upper) src
        setRef written (done + toWrite)


--------------------------------------------------------------------------------
-- Internal interface
--------------------------------------------------------------------------------

class Pipe p
  where
    initPipe     :: p a -> Host ()
    getCReadPtr  :: LocalRefAccess m => p a -> m (Data Index)
    getPReadPtr  :: LocalRefAccess m => p a -> m (Data Index)
    setReadPtr   :: LocalRefAccess m => p a -> Data Index -> m ()
    getCWritePtr :: LocalRefAccess m => p a -> m (Data Index)
    getPWritePtr :: LocalRefAccess m => p a -> m (Data Index)
    setWritePtr  :: LocalRefAccess m => p a -> Data Index -> m ()
    getSize      :: p a -> Data Length


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


instance Pipe (HostPipe dir)
  where
    initPipe (HostPipe rptr wptr _ _) = do
        setLocalRef rptr 0
        setLocalRef wptr 0
    getCReadPtr  (HostPipe rptr _ _ _) = getLocalRef rptr
    getPReadPtr  (HostPipe rptr _ _ _) = getLocalRef rptr
    setReadPtr   (HostPipe rptr _ _ _) = setLocalRef rptr
    getCWritePtr (HostPipe _ wptr _ _) = getLocalRef wptr
    getPWritePtr (HostPipe _ wptr _ _) = getLocalRef wptr
    setWritePtr  (HostPipe _ wptr _ _) = setLocalRef wptr
    getSize      (HostPipe _ _ _ size) = size


allocHostPipe :: SmallType a => CoreId -> Size -> Multicore (HostPipe dir a)
allocHostPipe coreId size = do
    rptr  <- allocRef coreId
    wptr  <- allocRef coreId
    elems <- allocSArr (size + 1)
    return $ HostPipe rptr wptr elems (value size + 1)


instance BulkPipeReader (HostPipe ToHost) Host
  where
    getElements (HostPipe _ _ elems _) i r a = readArrAt i elems r a

instance BulkPipeWriter (HostPipe FromHost) Host
  where
    setElements (HostPipe _ _ elems _) i r a = writeArrAt i elems r a


instance BulkPipeReader (HostPipe FromHost) CoreComp
  where
    getElements (HostPipe _ _ elems _) i r a = readArrAt i elems r a

instance BulkPipeWriter (HostPipe ToHost) CoreComp
  where
    setElements (HostPipe _ _ elems _) i r a = writeArrAt i elems r a


instance PipeReader (HostPipe FromHost) CoreComp
  where
    getElement (HostPipe _ _ elems _) i = do
        tmp <- newArr 1
        readArrAt i elems (0,0) tmp
        getArr 0 tmp

instance PipeWriter (HostPipe ToHost) CoreComp
  where
    setElement (HostPipe _ _ elems _) i e = do
        tmp <- newArr 1
        setArr 0 e tmp
        writeArrAt i elems (0,0) tmp


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


instance Pipe CorePipe
  where
    initPipe (CorePipe rptr wptr rptrS wptrS _ _) = do
        setLocalRef rptr  0
        setLocalRef wptr  0
        setLocalRef rptrS 0
        setLocalRef wptrS 0
    getCReadPtr (CorePipe rptr _ _ _ _ _)       = getLocalRef rptr
    getPReadPtr (CorePipe _ rptrS _ _ _ _)      = getLocalRef rptrS
    setReadPtr  (CorePipe rptr rptrS _ _ _ _) v = do
        setLocalRef rptr  v
        setLocalRef rptrS v
    getCWritePtr (CorePipe _ _ _ wptrS _ _)      = getLocalRef wptrS
    getPWritePtr (CorePipe _ _ wptr _ _ _)       = getLocalRef wptr
    setWritePtr  (CorePipe _ _ wptr wptrS _ _) v = do
        setLocalRef wptr  v
        setLocalRef wptrS v
    getSize (CorePipe _ _ _ _ _ size) = size


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
    getElement (CorePipe _ _ _ _ elems _) i = getArr i -< elems

instance PipeWriter CorePipe CoreComp
  where
    setElement (CorePipe _ _ _ _ elems _) i e = setArr i e -< elems


instance BulkPipeReader CorePipe CoreComp
  where
    getElements (CorePipe _ _ _ _ elems _) i r a = readArrAt i elems r a

instance BulkPipeWriter CorePipe CoreComp
  where
    setElements (CorePipe _ _ _ _ elems _) i r a = writeArrAt i elems r a
