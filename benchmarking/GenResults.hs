import Control.Monad
import System.Environment
import System.IO
import System.Random

main = do
    [ m ] <- getArgs
    withFile ("samples_" ++ m ++ ".dat") ReadMode $ \r -> do
        withFile ("results_" ++ m ++ ".dat") WriteMode $ \w -> do
            ls <- lines <$> hGetContents r
            forM_ ls $ \l -> do
                hPutStrLn w $ "> " ++ show (2 * (read l + 1))

