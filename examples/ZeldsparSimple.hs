module ZeldsparSimple where

import qualified Prelude

import Zeldspar.Multicore


simple :: Multicore ()
simple = do
    let chanSize = 10
    translatePar
        (inc `on` 0 |>>chanSize>>| twice `on` 1)
        readInput
        chanSize
        writeOutput
        chanSize
  where
    readInput :: Host (Data Int32, Data Bool)
    readInput = liftHost $ fget stdin >>= \i -> return (i, true)
    writeOutput :: Data Int32 -> Host (Data Bool)
    writeOutput o = printf "> %d\n" o >> return true

inc :: CoreZ (Data Int32) (Data Int32)
inc = zmap (+1)

twice :: CoreZ (Data Int32) (Data Int32)
twice = zmap (*2)

zmap :: (inp -> out) -> CoreZ inp out
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
    opts = defaultExtCompilerOpts {externalFlagsPost = ["-lpthread"]}