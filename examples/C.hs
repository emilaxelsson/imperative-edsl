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
  inModule "other" $ do
    addInclude "<stdlib.h>"
    addDefinition [cedecl|
      void func_in_other(void) {
        puts("Hello from the other module!");
      } |]
  addInclude "<stdlib.h>"
  addExternProc "func_in_other" []
  callProc "func_in_other" []
