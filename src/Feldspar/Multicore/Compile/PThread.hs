{-# LANGUAGE QuasiQuotes #-}
module Feldspar.Multicore.Compile.PThread where

import Control.Monad.Identity
import Control.Monad.Operational.Higher
import Data.Proxy
import Data.VirtualContainer

import Feldspar.Multicore.Compile.Platform
import Feldspar.Multicore.Representation
import Feldspar.Representation
import Feldspar.Run.Compile
import Feldspar.Run.Representation

import Language.C.Monad
import Language.C.Quote.C
import Language.Embedded.Imperative.CMD
import Language.Embedded.Backend.C.Expression


data PThread

pthread :: Platform PThread
pthread = Proxy


compAllocHostCMD :: CompExp exp
                 => AllocHostCMD exp (CGenTFor PThread Identity) a
                 -> CGenT Identity a
compAllocHostCMD cmd@(Alloc coreId size) = do
    sym <- gensym "spm"
    ty <- compTypeFromCMD cmd (proxyArg cmd)
    addGlobal [cedecl| volatile $ty:ty $id:sym[$size]; |]
    return $ Arr $ Actual $ ArrComp sym
compAllocHostCMD (OnHost host) = interpret $ lowerTop . liftRun $ runHost host

instance CompExp exp => CompFor (AllocHostCMD exp) PThread Identity
  where
    comp = compAllocHostCMD
    unwrap _ _ = runIdentity
