module Feldspar.Multicore.Compile.Parallella.Channel where

import Feldspar.Multicore.Compile.Parallella.Esdk
import Feldspar.Multicore.Compile.Parallella.Imports
import Feldspar.Multicore.Compile.Parallella.State


--------------------------------------------------------------------------------
-- Channel allocation
--------------------------------------------------------------------------------

compCoreChanAllocCMD :: forall a. CoreChanAllocCMD (Param3 RunGen Prim PrimType) a -> RunGen a
compCoreChanAllocCMD cmd@(NewChan _ _ _) = compNewCoreChan cmd

compNewCoreChan :: forall a. CoreChanAllocCMD (Param3 RunGen Prim PrimType) (CoreChan a) -> RunGen (CoreChan a)
compNewCoreChan cmd@(NewChan f t sz)
    | isCoreToCore = do
        let (r, c) = groupCoord t
            bty = cType (Proxy :: Proxy Bool)
            arg = proxyArg cmd
            cty = compType (proxyPred cmd) arg
        (isOpen :: DLArr Bool, _) <- allocLocal bty t 1
        (isFull :: DLArr Bool, _) <- allocLocal bty t 1
        (buf :: LArr (Data a), _) <- allocLocal cty t sz
        groupAddr <- gets group
        lift $ do
            addInclude "<feldspar-parallella.h>"
            callProc "init_core_chan"
                [ groupAddr
                , valArg $ value r
                , valArg $ value c
                , arrArg $ unwrapArr isOpen
                , arrArg $ unwrapArr isFull
                ]
        return $ CoreChanComp $ CoreChanRep
            [ arrArg $ unwrapArr buf, arrArg $ unwrapArr isOpen, arrArg $ unwrapArr isFull ]
    | otherwise = do
        let cid = if f == hostId then t else f
            (r, c) = groupCoord cid
            bty = cType (Proxy :: Proxy Bool)
            arg = proxyArg cmd
            cty = compType (proxyPred cmd) arg
        (isOpen :: DLArr Bool, _) <- allocLocal bty cid 1
        (isFull :: DLArr Bool, _) <- allocLocal bty cid 1
        (buf :: SArr (Data a), shmRef) <- allocShared cty sz
        groupAddr <- gets group
        hostChan <- lift $ do
            addInclude "<feldspar-parallella.h>"
            chan <- objArg <$> newNamedObject "chan" "host_chan_t" False
            callProc "init_host_chan"
                [ addr chan
                , groupAddr
                , valArg $ value r
                , valArg $ value c
                , shmRef
                , arrArg $ unwrapArr isOpen
                , arrArg $ unwrapArr isFull
                ]
            return chan
        return $ CoreChanComp $ HostChanRep hostChan
            [ arrArg $ unwrapArr buf, arrArg $ unwrapArr isOpen, arrArg $ unwrapArr isFull ]
    where
      isCoreToCore = f Prelude./= hostId Prelude.&& t Prelude./= hostId

instance Interp CoreChanAllocCMD RunGen (Param2 Prim PrimType)
  where interp = compCoreChanAllocCMD


--------------------------------------------------------------------------------
-- Host channels
--------------------------------------------------------------------------------

compHostCoreChanCMD :: CoreChanCMD (Param3 RunGen Data PrimType') a -> RunGen a
compHostCoreChanCMD (ReadChan (CoreChanComp (HostChanRep hostChan _)) off sz arr) = lift $ do
    addInclude "<feldspar-parallella.h>"
    callFun "host_read_c2h" [ hostChan, arrArg arr, valArg off, valArg sz ]
compHostCoreChanCMD (WriteOne (CoreChanComp (HostChanRep hostChan _)) v) = lift $ do
    addInclude "<feldspar-parallella.h>"
    callFun "host_write_h2c" [ hostChan, addr $ valArg v, valArg (0 :: Data Length), valArg (1 :: Data Length) ]
compHostCoreChanCMD (WriteChan (CoreChanComp (HostChanRep hostChan _)) off sz arr) = lift $ do
    addInclude "<feldspar-parallella.h>"
    callFun "host_write_h2c" [ hostChan, arrArg arr, valArg off, valArg sz ]
compHostCoreChanCMD (CloseChan (CoreChanComp (HostChanRep hostChan _))) = lift $ do
    addInclude "<feldspar-parallella.h>"
    callProc "host_close_chan" [ hostChan ]
compHostCoreChanCMD (CloseChan _) =
    error "closeChan: unable to close core-to-core channel in host"

instance Interp CoreChanCMD RunGen (Param2 Data PrimType')
  where interp = compHostCoreChanCMD


--------------------------------------------------------------------------------
-- Core channels
--------------------------------------------------------------------------------

compCoreChanCMD :: CoreChanCMD (Param3 CoreGen Data PrimType') a -> CoreGen a
compCoreChanCMD (ReadChan (CoreChanComp (HostChanRep _ chanArgs)) off sz arr) = lift $ do
    addInclude "<feldspar-parallella.h>"
    callFun "core_read_h2c" $ chanArgs ++ [ arrArg arr, valArg off, valArg sz ]
compCoreChanCMD (ReadChan (CoreChanComp (CoreChanRep chanArgs)) off sz arr) = lift $ do
    addInclude "<feldspar-parallella.h>"
    callFun "core_read_c2c" $ chanArgs ++ [ arrArg arr, valArg off, valArg sz ]
compCoreChanCMD (WriteOne (CoreChanComp (HostChanRep _ chanArgs)) v) = lift $ do
    addInclude "<feldspar-parallella.h>"
    callFun "core_write_c2h" $ chanArgs ++ [ addr $ valArg v, valArg (0 :: Data Length), valArg (1 :: Data Length) ]
compCoreChanCMD (WriteOne (CoreChanComp (CoreChanRep chanArgs)) v) = lift $ do
    addInclude "<feldspar-parallella.h>"
    callFun "core_write_c2c" $ chanArgs ++ [ addr $ valArg v, valArg (0 :: Data Length), valArg (1 :: Data Length) ]
compCoreChanCMD (WriteChan (CoreChanComp (HostChanRep _ chanArgs)) off sz arr) = lift $ do
    addInclude "<feldspar-parallella.h>"
    callFun "core_write_c2h" $ chanArgs ++ [ arrArg arr, valArg off, valArg sz ]
compCoreChanCMD (WriteChan (CoreChanComp (CoreChanRep chanArgs)) off sz arr) = lift $ do
    addInclude "<feldspar-parallella.h>"
    callFun "core_write_c2c" $ chanArgs ++ [ arrArg arr, valArg off, valArg sz ]
compCoreChanCMD (CloseChan (CoreChanComp (HostChanRep _ chanArgs))) = lift $ do
    addInclude "<feldspar-parallella.h>"
    callProc "core_close_chan" chanArgs
compCoreChanCMD (CloseChan (CoreChanComp (CoreChanRep chanArgs))) = lift $ do
    addInclude "<feldspar-parallella.h>"
    callProc "core_close_chan" chanArgs

instance Interp CoreChanCMD CoreGen (Param2 Data PrimType')
  where interp = compCoreChanCMD
