module Simple where

import Feldspar.Multicore


simple :: Multicore ()
simple = do
    d0 <- allocLArr 0 10
    d1 <- allocLArr 1 10
    d2 <- allocLArr 2 10
    onHost $ do
        input <- newArr 10
        for (0, 1, Incl 9) $ \i -> do
            printf "Item %d> " i
            item <- lift $ fget stdin
            setArr input i item

        writeArr d0 (0,9) input
        onCore 0 (f d0 d1)
        onCore 1 (g d1 d2)
        output <- newArr 10
        readArr d2 (0,9) output

        printf "Output:"
        for (0, 1, Incl 9) $ \i -> do
            item <- getArr output i
            printf " %d" item
        printf "\n"


f :: DLArr Int32 -> DLArr Int32 -> CoreComp ()
f input output = do
    for (0, 1, Incl 9) $ \i -> do
        item <- getLArr input i
        setLArr output i (item + 1)

g :: DLArr Int32 -> DLArr Int32 -> CoreComp ()
g input output = do
    for (0, 1, Incl 9) $ \i -> do
        item <- getLArr input i
        setLArr output i (item * 2)


------------------------------------------------------------

test = simple

testAll = do
    icompileAll `onParallella` test
    let modules = compileAll `onParallella` test
    forM_ modules $ \(name, contents) -> do
        let name' = if name Prelude.== "main" then "host" else name
        writeFile (name' Prelude.++ ".c") contents

runTestCompiled = runCompiled' def opts test
  where
    opts = def {externalFlagsPost = ["-lpthread"]}
