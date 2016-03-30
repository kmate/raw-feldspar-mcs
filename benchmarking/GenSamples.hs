import Control.Monad
import System.Environment
import System.IO
import System.Random

main = do
    [ n, m ] <- getArgs
    withFile ("samples_" ++ m ++ ".dat") WriteMode $ \h -> do
        void $ replicateM (read n - 1) $ do
            r <- randomRIO (1,255) :: IO Int
            hPutStrLn h $ show r
        hPutStrLn h "0"

