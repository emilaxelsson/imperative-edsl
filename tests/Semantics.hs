import Language.Embedded.Expression (evalExp)
import Language.Embedded.Imperative
import Language.Embedded.Expr



-- Test that `modifyRef` doesn't loop. It will loop if evaluation of `setRef` is
-- too lazy so that the `unsafeFreezeRef` happens before `setRef` in
-- `modifyRef`.
modifyRefProg :: Program (RefCMD Expr) (Expr Int32)
modifyRefProg = do
    r <- initRef 0
    modifyRef r (+1)
    getRef r

testModifyRef = do
    1 <- fmap evalExp $ interpret modifyRefProg
    return ()

main = do
    testModifyRef

