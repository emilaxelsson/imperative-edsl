{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

-- | C code generation for 'Program'

module Language.Embedded.Backend.C where



#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

import Data.Loc (noLoc)
import qualified Language.C.Syntax as C

import Control.Monad.Operational.Higher
import Language.C.Monad

import Text.PrettyPrint.Mainland (pretty)



namedType :: String -> C.Type
namedType t = C.Type
    (C.DeclSpec [] [] (C.Tnamed (C.Id t noLoc) [] noLoc) noLoc)
    (C.DeclRoot noLoc)
    noLoc

-- | Compile a program to C code represented as a string
--
-- For programs that make use of the primitives in
-- "Language.Embedded.Concurrent", the resulting C code can be compiled as
-- follows:
--
-- > gcc -Iinclude csrc/chan.c -lpthread YOURPROGRAM.c
compile :: (Interp instr CGen, HFunctor instr) => Program instr a -> String
compile = pretty 80 . prettyCGen . liftSharedLocals . wrapMain . interpret

-- | Compile a program to C code and print it on the screen
--
-- For programs that make use of the primitives in
-- "Language.Embedded.Concurrent", the resulting C code can be compiled as
-- follows:
--
-- > gcc -Iinclude csrc/chan.c -lpthread YOURPROGRAM.c
icompile :: (Interp instr CGen, HFunctor instr) => Program instr a -> IO ()
icompile = putStrLn . compile

