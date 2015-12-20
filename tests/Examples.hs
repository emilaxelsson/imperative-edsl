import qualified Imperative
import qualified Concurrent
import Language.Embedded.CExp
import Language.Embedded.Imperative
import Language.Embedded.Backend.C

test_strArg = flip (runCompiled []) [] $ do
    addInclude "<stdio.h>" :: Program (CallCMD CExp) ()
    callProc "printf" [strArg "Hello World!\n"]

main = do
    Imperative.testAll
    Concurrent.testAll
    test_strArg

