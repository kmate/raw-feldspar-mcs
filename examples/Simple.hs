module Simple where

import Feldspar
import Feldspar.Run
import Feldspar.Multicore


simple :: AllocHost ()
simple = do
    d0 <- alloc 10
    d1 <- alloc 10
    d2 <- alloc 10
    runHost $ do
        input :: Arr Int32 <- newArr 10
        for (0, 1, Incl 9) $ \(i :: Data Word32) -> do
            printf "Item %d> " i
         -- item <- fget stdin
         -- FIXME: fget should return `MonadRun m => m (Data a)`
         --        instead of `Run (Data a)`?
            item :: Data Int32 <- undefined
            setArr i item input

        fetch d0 (0,9) input
        onCore (f d0 d1)
        onCore (g d1 d2)
        output <- newArr 10
        flush d2 (0,9) output

        printf "Output:"
        for (0, 1, Incl 9) $ \i -> do
            item :: Data Int32 <- getArr i output
            printf " %d" item
        printf "\n"


f :: LocalArr 0 Int32 -> LocalArr 1 Int32 -> CoreComp 0 ()
f input output =
    for (0, 1, Incl 9) $ \i -> do
        item :: Data Int32 <- unsafeGetLArr i input
     -- Reading from a non-local array is forbidden
     -- item' :: Data Int32 <- unsafeGetLArr i output
        setLArr i (item + 1) output

g :: LocalArr 1 Int32 -> LocalArr 2 Int32 -> CoreComp 1 ()
g input output =
    for (0, 1, Incl 9) $ \i -> do
        item :: Data Int32 <- unsafeGetLArr i input
        setLArr i (item * 2) output
