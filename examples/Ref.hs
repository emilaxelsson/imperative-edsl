module Ref where



import Control.Monad.Operational.Compositional
import Language.C.Monad
import Language.Embedded.Imperative

import Exp



prog1 :: pred Int => Program (Tag pred Exp (RefCMD pred Exp :+: ArrCMD pred Exp)) (Exp Int)
prog1 = do
    ref <- initRef (Lit 4)
    arr <- newArr (Lit 10) (Lit 6)
    a   <- unsafeFreezeRef ref
    b   <- getArr (Lit 3) arr
    let c = Add a b
    setRef ref c
    return c

eval1 :: IO Int
eval1 = fmap evalExp $ interpret prog1

comp1 = prettyCGen $ wrapMain $ interpret prog1

