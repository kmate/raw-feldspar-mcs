module Zeldspar.Multicore.Compile where

import Feldspar.Multicore
import Zeldspar.Parallel

{-
-- FIXME: use something else related to Transferable instead of SmallType?
parZ :: forall inp out. (SmallType inp, SmallType out)
     => ParZ inp out ()
     -> Host (Data inp, Data Bool)      -- ^ Source
     -> (Data out -> Host (Data Bool))  -- ^ Sink
     -> Multicore ()
parZ ps inp out= do
    i :: HostToCorePipe inp <- allocHostPipe 0 10
    o :: CoreToHostPipe out <- allocHostPipe 1 10
    onHost $ do
        initPipe i
        initPipe o
        
        undefined -- TODO: implement the translation with Pipes
        -- use foldPP from Ziria.Parallel if possible
-}

translatePar :: forall inp out. (Transferable inp, Transferable out)
             => ParZun inp out ()
             -> (Host (inp, Data Bool))    -- ^ Source
             -> (out -> Host (Data Bool))  -- ^ Sink
             -> Multicore ()
translatePar = undefined

