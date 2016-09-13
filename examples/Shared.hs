module Shared where

import qualified Prelude

import Feldspar.Multicore

n' :: Word32
n' = 4

n :: Data Word32
n = value n'

shared :: Multicore ()
shared = do
    s0 <- allocSem 0
    b0 <- allocSArr n'
    s1 <- allocSem 1
    b1 <- allocLArr 1 n'
    s2 <- allocSem 2
    b2 <- allocSArr n'
    onHost $ do
        initSem s0
        initSem s1
        initSem s2
        onCore 0 (f (s0, b0) (s1, b1))
        onCore 1 (g (s1, b1) (s2, b2))

        forever $ do
            input <- newArr n
            for (0, 1, Excl n) $ \i -> do
                item <- lift $ fget stdin
                setArr input i item

            writeArr b0 (0, n - 1) input
            release s0

            output <- newArr n
            acquire s2
            readArr b2 (0, n - 1) output

            for (0, 1, Excl n) $ \i -> do
                item <- getArr output i
                printf "> %d\n" item


f :: (Sem, DSArr Int32) -> (Sem, DLArr Int32) -> CoreComp ()
f (ri, input) (ro, output) = forever $ do
    acquire ri

    tmp <- newArr n
    readArr input (0, n - 1) tmp
    for (0, 1, Excl n) $ \i -> do
        item <- getArr tmp i
        setLArr output i (item + 1)

    release ro

g :: (Sem, DLArr Int32) -> (Sem, DSArr Int32) -> CoreComp ()
g (ri, input) (ro, output) = forever $ do
    acquire ri

    tmp <- newArr n
    for (0, 1, Excl n) $ \i -> do
        item <- getLArr input i
        setArr tmp i (item * 2)
    writeArr output (0, n - 1) tmp

    release ro


------------------------------------------------------------

test = shared

testAll = do
    icompileAll `onParallella` test
    let modules = compileAll `onParallella` test
    forM_ modules $ \(name, contents) -> do
        let name' = if name Prelude.== "main" then "host" else name
        writeFile (name' Prelude.++ ".c") contents

runTestCompiled = runCompiled' def opts test
  where
    opts = def {externalFlagsPost = ["-lpthread"]}
