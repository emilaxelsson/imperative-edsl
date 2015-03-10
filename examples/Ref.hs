module Ref where



import Control.Monad.Operational.Compositional
import Language.C.Monad
import Language.Embedded.Imperative
import Language.Embedded.Expr



type Instr pred
    =   RefCMD pred Expr
    :+: ArrCMD pred Expr
    :+: ControlCMD Expr

prog1 :: pred Int => Program (Tag pred Expr (Instr pred)) (Expr Int)
prog1 = do
    ref <- initRef 4
    arr <- newArr 10 6
    a   <- unsafeFreezeRef ref
    b   <- getArr 3 arr
    let c = Add a b
    iff (Eq a 4)
      (setRef ref c)
      (setRef ref b)
    return c

eval1 :: IO Int
eval1 = fmap evalExp $ interpret prog1

comp1 = prettyCGen $ wrapMain $ interpret prog1

