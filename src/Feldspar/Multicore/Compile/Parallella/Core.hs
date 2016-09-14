module Feldspar.Multicore.Compile.Parallella.Core where

import Feldspar.Multicore.Compile.Parallella.Imports
import Feldspar.Multicore.Compile.Parallella.State
import Feldspar.Multicore.Compile.Parallella.Util


--------------------------------------------------------------------------------
-- Halt
--------------------------------------------------------------------------------

compCoreHaltCMD :: CoreHaltCMD (Param3 CoreGen exp pred) a -> CoreGen a
compCoreHaltCMD (HaltCore _) = lift $ callProc "core_halt" []

instance Interp CoreHaltCMD CoreGen (Param2 exp pred)
  where interp = compCoreHaltCMD


--------------------------------------------------------------------------------
-- Local array fetch and flush
--------------------------------------------------------------------------------

compCoreLocalBulkArrCMD :: (BulkArrCMD LArr) (Param3 CoreGen exp pred) a -> CoreGen a
compCoreLocalBulkArrCMD (WriteArr offset spm range ram) =
    compCoreLocalCopy "core_write_local"  spm ram offset range
compCoreLocalBulkArrCMD (ReadArr  offset spm range ram) =
    compCoreLocalCopy "core_read_local"   spm ram offset range

instance Interp (BulkArrCMD LArr) CoreGen (Param2 exp pred)
  where interp = compCoreLocalBulkArrCMD

compCoreLocalCopy :: PrimType a => String
                  -> DLArr a -> Arr (Data a)
                  -> Data Index -> IndexRange -> CoreGen ()
compCoreLocalCopy op spm ram offset (lower, upper) = do
    groupAddr <- asks group
    (r, c) <- asks $ groupCoordsForName (arrayRefName spm)
    lift $ do
        addInclude "<string.h>"
        addInclude "<e-lib.h>"
        addInclude "<feldspar-parallella.h>"
        callProc op
            [ arrArg (unwrapArr spm)
            , arrArg ram
            , valArg offset
            , valArg lower
            , valArg upper
            ]


--------------------------------------------------------------------------------
-- Shared array fetch and flush
--------------------------------------------------------------------------------

compCoreSharedBulkArrCMD :: (BulkArrCMD SArr) (Param3 CoreGen exp pred) a -> CoreGen a
compCoreSharedBulkArrCMD (WriteArr offset spm range ram) =
    compCoreSharedCopy "core_write_shared" spm ram offset range
compCoreSharedBulkArrCMD (ReadArr  offset spm range ram) =
    compCoreSharedCopy "core_read_shared"  spm ram offset range

instance Interp (BulkArrCMD SArr) CoreGen (Param2 exp pred)
  where interp = compCoreSharedBulkArrCMD

compCoreSharedCopy :: PrimType a => String
                   -> DSArr a -> Arr (Data a)
                   -> Data Index -> IndexRange -> CoreGen ()
compCoreSharedCopy op spm ram offset (lower, upper) = do
    shmRef <- asks $ shmRefForName $ arrayRefName spm
    lift $ do
        addInclude "<e-lib.h>"
        addInclude "<feldspar-parallella.h>"
        callProc op
            [ arrArg (unwrapArr spm)
            , arrArg ram
            , valArg offset
            , valArg lower
            , valArg upper
            ]
