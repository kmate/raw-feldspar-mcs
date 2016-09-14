module Feldspar.Multicore.Compile.Parallella where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Feldspar.Multicore.Compile.Parallella.Imports
import Feldspar.Multicore.Compile.Parallella.Multicore
import Feldspar.Multicore.Compile.Parallella.State

import Feldspar.Run.Compile (compileAll)


--------------------------------------------------------------------------------
-- Main compilation functions
--------------------------------------------------------------------------------

icompileAll :: MonadRun m => m a -> IO ()
icompileAll  = mapM_ (\(n, m) -> putStrLn ("// module " ++ n) >> putStrLn m) . compileAll

onParallella :: (Run a -> b) -> Multicore a -> b
onParallella action
    = action
    . wrapESDK
    . interpret
    . unMulticore


--------------------------------------------------------------------------------
-- Transformation over Run
--------------------------------------------------------------------------------

-- TODO: add only the required number of cores to the group?
wrapESDK :: RunGen a -> Run a
wrapESDK program = do
    addInclude "<e-hal.h>"
    groupAddr <- addr . objArg <$> newNamedObject "group" "e_epiphany_t" False
    callProc "e_init" [ valArg (value 0 :: Data Int32) {- NULL -} ]
    callProc "e_reset_system" []
    callProc "e_open" [ groupAddr
                      , valArg (value 0 :: Data Int32)
                      , valArg (value 0 :: Data Int32)
                      , valArg (value 4 :: Data Int32)
                      , valArg (value 4 :: Data Int32) ]
    callProc "e_reset_group" [ groupAddr ]
    (result, state) <- runGen (start groupAddr) program
    let sharedTypeIncludes = fromMaybe Set.empty (Map.lookup sharedId (inclMap state))
    mapM_ addInclude sharedTypeIncludes
    mapM_ (callProc "e_free" . return . snd) (Map.toList $ shmMap state)
    callProc "e_reset_group" [ groupAddr ]
    callProc "e_close" [ groupAddr ]
    callProc "e_finalize" []
    return result
