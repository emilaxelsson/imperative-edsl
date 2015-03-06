module Ref where



import Control.Monad.Operational

import Language.C.Monad
import Language.Embedded.Imperative

import Exp



prog1 :: pred Int => Program (RefCMD pred Exp) (Exp Int)
prog1 = do
    r1 <- initRef (Lit 4)
    r2 <- initRef (Lit 5)
    a <- unsafeFreezeRef r1
    b <- getRef r2
    let c = Add a b
    setRef r2 c
    return c

eval1 = fmap evalExp $ interpretWithMonad runRefCMD prog1
comp1 = prettyCGen $ wrapMain $ interpretWithMonad compRefCMD prog1

