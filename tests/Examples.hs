import qualified Flags
import qualified Simple
import qualified Shared
import qualified ZeldsparFFT
import qualified ZeldsparSimple
import qualified ZeldsparVector

main = do
    Flags.testAll
    Simple.testAll
    Shared.testAll
    ZeldsparFFT.testAll
    ZeldsparSimple.testAll
    ZeldsparVector.testAll
