import qualified Imperative
import qualified Concurrent
import qualified Demo
import qualified CExp

main = do
    Imperative.testAll
    Concurrent.testAll
    Demo.testAll
    CExp.main

