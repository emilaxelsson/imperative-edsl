import qualified Imperative
import qualified Concurrent
import qualified Demo

main = do
    Imperative.testAll
    Concurrent.testAll
    Demo.testAll

