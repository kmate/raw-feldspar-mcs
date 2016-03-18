module Shared where

import Feldspar.Multicore


shared :: Multicore ()
shared = do
    d0 :: SharedArr Int32 <- allocSArr 10
    d1 :: SharedArr Int32 <- allocSArr 10
    d2 :: SharedArr Int32 <- allocSArr 10
    onHost $ do
        input :: Arr Int32 <- newArr 10
        for (0, 1, Incl 9) $ \i -> do
            printf "Item %d> " i
            item <- lift $ fget stdin
            setArr i item input

        writeArr d0 (0,9) input
     -- onCore 0 (f d0 d1)
     -- onCore 1 (g d1 d2)
        output <- newArr 10
        readArr d0 (0,9) output
     -- readArr d2 (0,9) output

        printf "Output:"
        for (0, 1, Incl 9) $ \i -> do
            item :: Data Int32 <- getArr i output
            printf " %d" item
        printf "\n"

{-
f :: SharedArr Int32 -> SharedArr Int32 -> CoreComp ()
f input output = do
    for (0, 1, Incl 9) $ \i -> do
        item :: Data Int32 <- getArr i -< input
        setArr i (item + 1) -< output

g :: SharedArr Int32 -> SharedArr Int32 -> CoreComp ()
g input output = do
    for (0, 1, Incl 9) $ \i -> do
        item :: Data Int32 <- getArr i -< input
        setArr i (item * 2) -< output
-}

------------------------------------------------------------

test = shared

testAll = do
    icompileAll `onParallella` test
    let modules = compileAll `onParallella` test
    forM_ modules $ \(name, contents) -> do
        let name' = if name Prelude.== "main" then "host" else name
        writeFile (name' Prelude.++ ".c") contents

runTestCompiled = runCompiled' opts test
  where
    opts = defaultExtCompilerOpts {externalFlagsPost = ["-lpthread"]}
