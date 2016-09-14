module Feldspar.Multicore.Compile.Parallella.Multicore where

import Feldspar.Multicore.Compile.Parallella.Access
import Feldspar.Multicore.Compile.Parallella.Host
import Feldspar.Multicore.Compile.Parallella.Imports
import Feldspar.Multicore.Compile.Parallella.State


--------------------------------------------------------------------------------
-- Allocation layer
--------------------------------------------------------------------------------

-- TODO: allocate only the arrays that are really used?
compAllocCMD :: (CompExp Prim, CompTypeClass PrimType)
             => AllocCMD (Param3 RunGen Prim PrimType) a -> RunGen a
compAllocCMD cmd@(AllocSArr size) = do
    let cty = compType (proxyPred cmd) (proxyArg (proxyArg cmd))
    fst <$> allocShared cty size
compAllocCMD cmd@(AllocLArr coreId size) = do
    let cty = compType (proxyPred cmd) (proxyArg (proxyArg cmd))
    fst <$> allocLocal cty coreId size
compAllocCMD (OnHost host) = do
    s <- get
    lift $ evalGen s $ interpretT (lift :: Run a -> RunGen a) $ unHost host

instance Interp AllocCMD RunGen (Param2 Prim PrimType)
  where interp = compAllocCMD
