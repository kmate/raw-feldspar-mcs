import qualified Flags
import qualified GeneralPipes
import qualified Pipes
import qualified Simple

main = do
    Flags.testAll
    GeneralPipes.testAll
    Pipes.testAll
    Simple.testAll

