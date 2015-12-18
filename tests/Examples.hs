import Imperative ()
import Concurrent ()
import Language.Embedded.CExp
import Language.Embedded.Imperative
import Language.Embedded.Backend.C

test_strArg = flip (runCompiled []) [] $ do
    addInclude "<stdio.h>" :: Program (CallCMD CExp) ()
    callProc "printf" [strArg "Hello World!\n"]

main = test_strArg

