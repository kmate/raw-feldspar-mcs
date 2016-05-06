module ZeldsparSimple where

import qualified Prelude

import Zeldspar.Multicore

-- TODO: reimplement example Simple with multicore Zeldspar

simple :: Multicore ()
simple = translatePar (inc `on` 0 |>>10>>| twice `on` 1) readInput writeOutput
  where
    readInput :: Host (Data Int32, Data Bool)
    readInput = do
        i <- liftHost $ fget stdin
        return (i, true)
    writeOutput :: Data Int32 -> Host (Data Bool)
    writeOutput o = do
        printf "> %d\n" o
        return true

inc :: CoreZ (Data Int32) (Data Int32)
inc = zmap (+1)

twice :: CoreZ (Data Int32) (Data Int32)
twice = zmap (*2)

zmap :: (inp -> out) -> CoreZ inp out
zmap f = do
    x <- receive
    emit (f x)

doubleHost :: Multicore ()
doubleHost = do
    allocLArr 0 10 :: Multicore (LocalArr Int32)
    onHost $ printf "A\n"
    allocLArr 1 10 :: Multicore (LocalArr Int32)
    onHost $ printf "B\n"


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
