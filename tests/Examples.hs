import qualified Channel
import qualified Simple
import qualified Shared
import qualified ZeldsparFFT
import qualified ZeldsparSimple
import qualified ZeldsparVector

main = do
    Channel.testAll
    Simple.testAll
    Shared.testAll
    ZeldsparFFT.testAll
    ZeldsparSimple.testAll
    ZeldsparVector.testAll
