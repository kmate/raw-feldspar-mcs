module Feldspar.Multicore.Pipe
  ( HostToCorePipe, CorePipe, CoreToHostPipe
  , allocHostPipe, allocCorePipe
  , Pipe, initPipe
  , PipeReader, readPipeA, readPipe
  , PipeWriter, writePipeA, writePipe
  , BulkPipeReader, pullPipeA, pullPipe
  , BulkPipeWriter, pushPipeA, pushPipe
  ) where

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
    getElement :: PrimType a => p a -> Data Index -> m (Data a)

-- | Asynchronously reads an element from a pipe.
--   Immediately gives None, when the pipe is emty.
--   Otherwise it returns the element read.
readPipeA :: (Pipe p, PipeReader p m, PrimType a)
          => p a -> OptionT m (Data a)
readPipeA pipe = do
    let size = getSize pipe
    rx <- lift $ getCReadPtr  pipe
    wx <- lift $ getCWritePtr pipe
    guarded "no data" (rx /= wx) ()
    lift $ do
        rx <- getCReadPtr pipe
        elem <- getElement pipe rx
        setReadPtr pipe ((rx + 1) `mod'` size)
        return elem

-- | Synchronously reads an element from a pipe. Blocks until an element found.
readPipe  :: (Pipe p, Wait m, PipeReader p m, PrimType a)
          => p a -> m (Data a)
readPipe pipe = do
        value <- newRef
        noValue <- initRef true
        while (getRef noValue) $ do
            caseOptionT (readPipeA pipe)
                (const $ busyWait)
                (setRef value >=> (const $ setRef noValue false))
        getRef value


class (Pipe p, LocalRefAccess m) => PipeWriter p m
  where
    setElement :: PrimType a => p a -> Data Index -> Data a -> m ()

-- | Asynchronously writes an element into a pipe.
--   Immediately returns False, when the pipe is full.
--   Otherwise it returns True after the element is written into the pipe.
writePipeA :: (Pipe p, PipeWriter p m, PrimType a)
           => Data a -> p a -> m (Data Bool)
writePipeA elem pipe = do
    let size = getSize pipe
    rx <- getPReadPtr  pipe
    wx <- getPWritePtr pipe
    done <- initRef false
    let wx' = (wx + 1) `mod'` size
    iff (wx' == rx)
        (setRef done false)
        (do
            wx <- getPWritePtr pipe
            setElement pipe wx elem
            setWritePtr pipe wx'
            setRef done true)
    getRef done

-- | Synchronously writes an element into a pipe. Blocks until the write succeeds.
writePipe  :: (Pipe p, Wait m, PipeWriter p m, PrimType a) => Data a -> p a -> m ()
writePipe elem pipe = while (not <$> writePipeA elem pipe) $ busyWait


class (Pipe p, LocalRefAccess m) => BulkPipeReader p m
  where
    getElements :: PrimType a => p a -> Data Index -> IndexRange -> Arr a -> m ()

-- | Asynchronously reads multiple elements from a pipe.
--   Immediately returns the number of elements read,
--   when the pipe is not empty.
pullPipeA :: (Pipe p, Wait m, BulkPipeReader p m, PrimType a)
          => p a -> IndexRange -> Arr a -> m (Data Index)
pullPipeA pipe (lower, upper) dst = do
    -- wait for items in the buffer
    while (do
        rx <- getCReadPtr pipe
        wx <- getCWritePtr pipe
        return $ rx == wx) $ busyWait
    -- calculate items left
    let left = upper - lower + 1
    -- how many items could it read in this round
    rx <- getCReadPtr pipe
    wx <- getCWritePtr pipe
    size <- force $ getSize pipe
    available <- force $ (size + wx -rx) `mod'` size
    toRead <- force $ min left available
    let start = lower
    iff (rx + toRead <= size)
        (getElements pipe rx (start, start + toRead - 1) dst)
        (do toEnd <- force $ size - rx
            getElements pipe rx (start, start + toEnd - 1) dst
            start' <- force $ start + toEnd
            getElements pipe 0 (start', start' + toRead - toEnd - 1) dst)
    rx' <- force $ (rx + toRead) `mod'` size
    setReadPtr pipe rx'
    return toRead

-- | Synchronously reads multple elements from a pipe.
--   Blocks until all the elements in the given range are read.
pullPipe  :: (Pipe p, Wait m, BulkPipeReader p m, PrimType a)
          => p a -> IndexRange -> Arr a -> m ()
pullPipe pipe (lower, upper) dst = do
    let total = upper - lower + 1
    read <- initRef (value 0 :: Data Length)
    -- until all data is read
    while ((<total) <$> getRef read) $ do
        done <- getRef read
        toRead <- pullPipeA pipe (lower + done, upper) dst
        setRef read (done + toRead)
    -- reset pointers for performance if needed
    rx <- getCReadPtr  pipe
    wx <- getCWritePtr pipe
    iff (rx == wx && wx == getSize pipe - 1)
        (do setReadPtr  pipe 0
            setWritePtr pipe 0)
        (return ())


class (Pipe p, LocalRefAccess m) => BulkPipeWriter p m
  where
    setElements :: PrimType a => p a -> Data Index -> IndexRange -> Arr a -> m ()

-- | Asynchronously writes multiple elements into a pipe.
--   Immediately returns the number of elements written,
--   when the pipe is not full.
pushPipeA :: (Pipe p, Wait m, BulkPipeWriter p m, PrimType a)
          => p a -> IndexRange -> Arr a -> m (Data Index)
pushPipeA pipe (lower, upper) src = do
    let size = getSize pipe
    -- wait for empty space in the buffer
    while (do
        rx <- getPReadPtr  pipe
        wx <- getPWritePtr pipe
        return $ (wx + 1) `mod'` size == rx) $ busyWait
    -- calculate items left
    let left = upper - lower + 1
    -- how many items could be writen in this round
    rx <- getPReadPtr  pipe
    wx <- getPWritePtr pipe
    available <- force $ (size + wx - rx) `mod'` size
    empty <- force $ size - available - 1  -- one slot is reserved in this setup
    toWrite <- force $ min left empty
    let start = lower
    iff (wx + toWrite <= size)
        (setElements pipe wx (start, start + toWrite - 1) src)
        (do toEnd <- force $ size - wx
            setElements pipe wx (start, start + toEnd - 1) src
            start' <- force $ start + toEnd
            setElements pipe 0 (start', start' + (toWrite - toEnd) - 1) src)
    wx' <- force $ (wx + toWrite) `mod'` size
    setWritePtr pipe wx'
    return toWrite

-- | Synchronously writes multiple elements into a pipe.
--   Blcoks untipl all the elements in the given range are written.
pushPipe  :: (Pipe p, Wait m, BulkPipeWriter p m, PrimType a)
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
-- Performance utility
--------------------------------------------------------------------------------

mod' :: (PrimType a, Integral a) => Data a -> Data a -> Data a
a `mod'` m = (a < m) ? a $ a - m


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


allocHostPipe :: PrimType a => CoreId -> Size -> Multicore (HostPipe dir a)
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


allocCorePipe :: PrimType a => CoreId -> CoreId -> Size -> Multicore (CorePipe a)
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
