module Pipeline where

import qualified Prelude
import GHC.TypeLits

import Feldspar.Multicore


n :: Word32
n = 4

pipeline :: Multicore ()
pipeline = do
    r0 <- alloc 1
    a0 <- alloc n
    r1 <- alloc 1
    a1 <- alloc n
    r2 <- alloc 1
    a2 <- alloc n
    onHost $ do
        check <- initArr [False]
        reset <- initArr [False]
        set <- initArr [True]
        fetch r0 (0,0) reset
        fetch r1 (0,0) reset
        fetch r2 (0,0) reset
        onCore (f (r0, a0) (r1, a1) :: CoreComp 0 ())
        onCore (g (r1, a1) (r2, a2))

        while (return $ true) $ do
            input :: Arr Int32 <- newArr $ value n
            for (0, 1, Excl $ value n) $ \i -> do
                item :: Data Int32 <- lift $ fget stdin
                setArr i item input

            fetch a0 (0, value $ n - 1) input
            fetch r0 (0,0) set
            output <- newArr $ value n
            while (not <$> (flush r2 (0,0) check >> getArr 0 check)) $ return ()
            fetch r2 (0,0) reset
            flush a2 (0, value $ n - 1) output

            for (0, 1, Excl $ value n) $ \i -> do
                item :: Data Int32 <- getArr i output
                lift $ printf "> %d\n" item


f :: (KnownNat a, KnownNat (a + 1))
  => (LocalArr a Bool, LocalArr a Int32) -> (LocalArr (a + 1) Bool, LocalArr (a + 1) Int32) -> CoreComp a ()
f (ri, input) (ro, output) = while (return $ true) $ do
    while (not <$> getLArr 0 ri) $ return ()
    setLArr 0 false ri

    for (0, 1, Excl $ value n) $ \i -> do
        item :: Data Int32 <- getLArr i input
        setLArr i (item + 1) output
    setLArr 0 true ro

g :: (KnownNat a, KnownNat (a + 1))
  => (LocalArr a Bool, LocalArr a Int32) -> (LocalArr (a + 1) Bool, LocalArr (a + 1) Int32) -> CoreComp a ()
g (ri, input) (ro, output) = while (return $ true) $ do
    while (not <$> getLArr 0 ri) $ return ()
    setLArr 0 false ri

    for (0, 1, Excl $ value n) $ \i -> do
        item :: Data Int32 <- getLArr i input
        setLArr i (item * 2) output
    setLArr 0 true ro


------------------------------------------------------------

testAll = icompileAll `onParallella` pipeline
