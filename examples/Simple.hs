module Simple where

import Feldspar.Multicore

import GHC.TypeLits


simple :: Multicore ()
simple = do
    d0 <- allocArr 10
    d1 <- allocArr 10
    d2 <- allocArr 10
    onHost $ do
        input :: Arr Int32 <- newArr 10
        for (0, 1, Incl 9) $ \i -> do
            printf "Item %d> " i
            item <- lift $ fget stdin
            setArr i item input

        writeArr d0 (0,9) input
        onCore (f d0 d1)
        onCore ((g d1 d2) :: CoreComp 1 ())
        output <- newArr 10
        readArr d2 (0,9) output

        printf "Output:"
        for (0, 1, Incl 9) $ \i -> do
            item :: Data Int32 <- getArr i output
            printf " %d" item
        printf "\n"


f :: LocalArr 0 Int32 -> LocalArr 1 Int32 -> CoreComp 0 ()
f input output =
    for (0, 1, Incl 9) $ \i -> do
        item :: Data Int32 <- getArr i -< input
        setArr i (item + 1) -< output

g :: (KnownNat coreId, KnownNat (coreId + 1))
  => LocalArr coreId Int32 -> LocalArr (coreId + 1) Int32 -> CoreComp coreId ()
g input output =
    for (0, 1, Incl 9) $ \i -> do
        item :: Data Int32 <- getArr i -< input
        setArr i (item * 2) -< output


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
