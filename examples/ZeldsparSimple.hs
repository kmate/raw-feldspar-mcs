module ZeldsparSimple where

import qualified Prelude

import Zeldspar.Multicore

-- TODO: reimplement example Simple with multicore Zeldspar

simple :: Multicore ()
simple = translatePar (inc |>>>| twice) readInput writeOutput
  where
    readInput :: Host (Data Int32, Data Bool)
    readInput = undefined
    writeOutput :: Data Int32 -> Host (Data Bool)
    writeOutput = undefined

inc :: Zun (Data Int32) (Data Int32) ()
inc = zmap (+1)

twice :: Zun (Data Int32) (Data Int32) ()
twice = zmap (*2)

zmap :: (inp -> out) -> Zun inp out ()
zmap f = do
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

