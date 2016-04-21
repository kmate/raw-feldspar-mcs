import qualified BulkPipes
import qualified Flags
import qualified GeneralPipes
import qualified Pipes
import qualified Simple
import qualified Shared

main = do
    BulkPipes.testAll
    Flags.testAll
    GeneralPipes.testAll
    Pipes.testAll
    Simple.testAll
    Shared.testAll
