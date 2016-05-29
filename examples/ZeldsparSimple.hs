module ZeldsparSimple where

import qualified Prelude

import Zeldspar.Multicore


simple :: Multicore ()
simple = do
    runZ
        (inc `on` 0 |>>>| twice `on` 1)
        readInput
        one
        writeOutput
        one
  where
    readInput :: Host (Data Int32, Data Bool)
    readInput = liftHost $ fget stdin >>= \i -> return (i, true)
    writeOutput :: Data Int32 -> Host (Data Bool)
    writeOutput o = printf "> %d\n" o >> return true

inc :: (PrimType a, Num a) => CoreZ (Data a) (Data a) ()
inc = zmap (+1)

twice :: (PrimType a, Num a) => CoreZ (Data a) (Data a) ()
twice = zmap (*2)

zmap :: (inp -> out) -> CoreZ inp out ()
zmap f = loop $ do
    x <- receive
    emit (f x)


------------------------------------------------------------

test = simple

testAll = do
    icompileAll `onParallella` test
    let modules = compileAll `onParallella` test
    forM_ modules $ \(name, contents) -> do
        let name' = if name Prelude.== "main" then "host" else name
        writeFile (name' Prelude.++ ".c") contents

runTestCompiled = runCompiled' opts test
  where
    opts = defaultExtCompilerOpts
        { externalFlagsPre  = [ "-I../imperative-edsl/include"
                              , "../imperative-edsl/csrc/chan.c"]
        , externalFlagsPost = [ "-lpthread" ]
        }
