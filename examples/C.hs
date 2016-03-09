{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

module C where

import Prelude hiding (break)

import Language.C.Quote.C
import Language.Embedded.Imperative
import Language.Embedded.Concurrent
import Language.Embedded.Backend.C
import Language.Embedded.CExp

type L =
  C_CMD CExp :+:
  FileCMD CExp

-- | Define a function in another module and call it.
multiModule :: Program L ()
multiModule = do
  addInclude "<stdlib.h>"
  addExternProc "func_in_other" []
  inModule "other" $ do
    addDefinition [cedecl|
      void func_in_other(void) {
        puts("Hello from the other module!");
      } |]
    addInclude "<stdio.h>"
  callProc "func_in_other" []

----------------------------------------

testAll = do
    icompileAll multiModule
