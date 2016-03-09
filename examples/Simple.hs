module Simple where

import Feldspar.Multicore

import GHC.TypeLits


simple :: AllocHost ()
simple = do
    d0 <- alloc 10
    d1 <- alloc 10
    d2 <- alloc 10
    onHost $ do
        input :: Arr Int32 <- newArr 10
        for (0, 1, Incl 9) $ \(i :: Data Word32) -> do
            printf "Item %d> " i
            item :: Data Int32 <- lift $ fget stdin
            setArr i item input

        fetch d0 (0,9) input
        onCore (f d0 d1)
        onCore ((g d1 d2) :: CoreComp 1 ())
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
        item :: Data Int32 <- getLArr i input
     -- Reading from a non-local array is forbidden
     -- item' :: Data Int32 <- getLArr i output
        setLArr i (item + 1) output

g :: forall (coreId :: Nat).
     LocalArr coreId Int32 -> LocalArr (coreId + 1) Int32 -> CoreComp coreId ()
g input output =
    for (0, 1, Incl 9) $ \i -> do
        item :: Data Int32 <- getArr i input
        setArr i (item * 2) output


------------------------------------------------------------

testAll = icompileAll `onParallella` simple
