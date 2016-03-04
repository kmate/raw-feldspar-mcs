module Simple where

import Feldspar.Multicore


simple :: AllocHost ()
simple = do
    d0 <- alloc 0 10
    d1 <- alloc 1 10
    d2 <- alloc 2 10
    runHost $ do
        input :: Arr Int32 <- newArr 10
        for (0, 1, Incl 9) $ \(i :: Data Word32) -> do
            lift $ printf "Item %d> " i
            item :: Data Int32 <- lift $ fget stdin
            setArr i item input

        fetch d0 (0,9) input
        onCore 0 (f d0 d1)
        onCore 1 (g d1 d2)
        output <- newArr 10
        flush d2 (0,9) output

        lift $ printf "Output:"
        for (0, 1, Incl 9) $ \i -> do
            item :: Data Int32 <- getArr i output
            lift $ printf " %d" item
        lift $ printf "\n"


f :: Arr Int32 -> Arr Int32 -> Comp ()
f input output =
    for (0, 1, Incl 9) $ \i -> do
        item :: Data Int32 <- getArr i input
        setArr i (item + 1) output

g :: Arr Int32 -> Arr Int32 -> Comp ()
g input output =
    for (0, 1, Incl 9) $ \i -> do
        item :: Data Int32 <- getArr i input
        setArr i (item * 2) output
