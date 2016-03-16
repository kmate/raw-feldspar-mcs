module Simple where

import Feldspar.Multicore


simple :: Multicore ()
simple = do
    d0 <- allocArr 0 10
    d1 <- allocArr 1 10
    d2 <- allocArr 2 10
    onHost $ do
        input :: Arr Int32 <- newArr 10
        for (0, 1, Incl 9) $ \i -> do
            printf "Item %d> " i
            item :: Data Int32 <- lift $ fget stdin
            setArr i item input

        writeArr d0 (0,9) input
        onCore 0 (f d0 d1)
        onCore 1 (g d1 d2)
        output <- newArr 10
        readArr d2 (0,9) output

        printf "Output:"
        for (0, 1, Incl 9) $ \i -> do
            item :: Data Int32 <- getArr i output
            printf " %d" item
        printf "\n"


f :: Arr Int32 -> Arr Int32 -> CoreComp ()
f input output =
    for (0, 1, Incl 9) $ \i -> do
        item :: Data Int32 <- getArr i input
        setArr i (item + 1) output

g :: Arr Int32 -> Arr Int32 -> CoreComp ()
g input output =
    for (0, 1, Incl 9) $ \i -> do
        item :: Data Int32 <- getArr i input
        setArr i (item * 2) output


------------------------------------------------------------

testAll = icompileAll `onParallella` simple
