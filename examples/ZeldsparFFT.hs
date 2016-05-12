module ZeldsparFFT where

import qualified Prelude as P

import Zeldspar.Multicore


type RealSamples     = Vector (Data Double)
type ComplexSamples  = Vector (Data (Complex Double))
type Twiddles        = Vector (Data (Complex Double))


flipFlop :: Storable a
         => (Store a, Store a)
         -> [a -> a]
         -> CoreZ a a
flipFlop (a, b) fs = do
  let fs' = P.zip fs (P.cycle [(a, b), (b, a)])
      go _ (f, (src, dst)) = do
          input <- unsafeFreezeStore src
          let output = f input
          writeStore dst output
          return output
  loop $ do
    input <- receive
    lift $ writeStore a input
    output <- lift $ foldM go input fs'
    emit output


--------------------------------------------------------------------------------
-- Bit reversal
--------------------------------------------------------------------------------

-- | Rotates the 'k + 1' LSB bits right with 1.
rotBit :: Index -> Data Index -> Data Index
rotBit k i = lefts .|. rights
  where
    k'     = i2n $ value k
    ir     = i .>>. 1
    rights = ir .&. oneBits k'
    lefts  = (((ir .>>. k') .<<. 1) .|. (i .&. 1)) .<<. k'

-- | Permute the vector by applying 'rotBit k' on its indices.
riffle :: Index -> Vector (Data a) -> Vector (Data a)
riffle k = permute (const $ rotBit k)

-- | Generates parallel bit reversal of a vector with length 'n'.
bitRev :: PrimType a => Length -> [ Vector (Data a) -> Vector (Data a) ]
bitRev n = [ riffle k | k <- [1..P.floor (logBase 2 $ P.fromIntegral n) - 1] ]


--------------------------------------------------------------------------------
-- "Pull-style" parallel FFT
--------------------------------------------------------------------------------

fft :: Length -> CoreZ ComplexSamples ComplexSamples
fft = fftBase False

ifft :: Length -> CoreZ ComplexSamples ComplexSamples
ifft = fftBase True

-- Fusion of a whole FFT is not possible for 'n' = 1024,
-- so we store the internal result after each stage to the same store.
fftBase :: Bool ->  Length -> CoreZ ComplexSamples ComplexSamples
fftBase inv n = do
  core <- lift $ fftCore inv n
  let rev = bitRev n
      n'  = P.fromIntegral n
  a <- lift $ newStore n'
  b <- lift $ newStore n'
  flipFlop (a, b) (core P.++ rev)

-- | Generates all 'ilog2 n' FFT/IFFT stages for a sample vector.
fftCore :: Bool -> Length -> CoreComp [ ComplexSamples -> ComplexSamples ]
fftCore inv n = do
  twids <- precompute (twids inv n)
  let n' = P.floor (logBase 2 $ P.fromIntegral n) - 1
  return [ step twids n k | k <- P.reverse [0..n'] ]

-- | Performs the 'k'th FFT/IFFT stage on a sample vector.
step :: Twiddles -> Length -> Length -> ComplexSamples -> ComplexSamples
step twids n k v = Indexed (value n) ixf
  where
    ixf i = testBit i (i2n k') ? ((twids ! t) * (b - a)) $ (a + b)
      where
        a  = v ! i
        b  = v ! (i `xor` (1 .<<. i2n k'))
        k' = value k
        t  = lsbs (i2n $ value n') (i .<<. value p)
        p  = P.fromIntegral $ n' - k
        n' = P.floor (logBase 2 $ P.fromIntegral n) - 1

twids :: Bool -> Length -> Twiddles
twids inv n = Indexed (value (n `P.div` 2)) ixf
  where
    scale = if inv then 1 else -1
    ixf i = polar 1 (π * i2n i * value (scale * 2 P./ P.fromIntegral n))

-- | Indicates whether the 'i'th bit of 'a' is set.
testBit :: (Bits a, Num a, PrimType a) => Data a -> Data Index -> Data Bool
testBit a i = a .&. (1 .<<. i2n i) /= 0


{-
fft :: (RealFloat a, PrimType (Complex a))
    => Length
    -> CoreZ (Vector (Data (Complex a))) (Vector (Data (Complex a)))
fft n = loop $ do
    input <- receive
    emit $ map (+1) input
-}

testFFT :: String -> Multicore ()
testFFT inputFile = do
    let n = 8
        chanSize = 10 `ofLength` n
    (h, inp) <- onHost $ do
        h <- liftHost $ fopen inputFile ReadMode
        input :: Arr (Complex Double) <- newArr (value n)
        for (0, 1, Excl $ value n) $ \i -> do
            re :: Data Double <- liftHost $ fget h
            im :: Data Double <- liftHost $ fget h
            setArr i (complex re im) input
        inp <- unsafeFreezeVec (value n) input
        return (h, inp)
    translatePar
        (fft n `on` 0)
        (return (inp, true))
        chanSize
        (\output -> do
            let n = length output
            printf "%d\n" n
            for (0, 1, Excl n) $ \i -> do
                let xi :: Data (Complex Double) = output ! i
                    re = realPart xi
                    im = imagPart xi
                printf "%f %f\n" re im
            return true)
        chanSize
    onHost $ liftHost $ fclose h


------------------------------------------------------------

test = testFFT "FFT_in8c.txt"

testAll = do
    icompileAll `onParallella` test
    let modules = compileAll `onParallella` test
    forM_ modules $ \(name, contents) -> do
        let name' = if name P.== "main" then "host" else name
        writeFile (name' P.++ ".c") contents

runTestCompiled = runCompiled' opts test
  where
    opts = defaultExtCompilerOpts {externalFlagsPost = ["-lpthread"]}
