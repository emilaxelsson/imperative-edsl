-- | Deep embedding of imperative programs with code generation. This is the
-- main module for users who want to write imperative programs.
--
-- The 'Program' type is parameterized by an instruction set that can be
-- combined in a modular way; e.g:
--
-- @
-- type MyProg exp a = `Program` (`RefCMD` exp `:+:` `FileCMD` exp) a
-- @
--
-- Also, instructions are parameterized on the expression language.
--
-- Some examples of using the library are found in the @examples@ directory.

module Language.Embedded.Imperative
  ( -- * Program monad
    module Control.Monad
  , ProgramT
  , Program
  , interpretT
  , interpret
  , interpretBiT
  , interpretBi
  , Param1
  , Param2
  , Param3
    -- * Imperative instructions
  , RefCMD
  , ArrCMD
  , ControlCMD
  , PtrCMD
  , FileCMD
  , C_CMD
    -- * Composing instruction sets
  , (:+:)
  , (:<:)
    -- * Interface for expression types
  , FreeExp
  , FreePred
  , EvalExp
  , CompExp
    -- * Front end
  , module Data.Int
  , module Data.Word
  , module Language.Embedded.Imperative.Frontend.General
  , module Language.Embedded.Imperative.Frontend
  ) where



import Control.Monad
import Data.Int
import Data.Word

import Control.Monad.Operational.Higher

import Language.Embedded.Expression
import Language.Embedded.Imperative.CMD
import Language.Embedded.Imperative.Frontend.General
import Language.Embedded.Imperative.Frontend
import Language.Embedded.Backend.C.Expression
import Language.Embedded.Imperative.Backend.C ()

