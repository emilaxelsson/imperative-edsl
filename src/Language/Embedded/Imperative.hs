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
-- Also, instructions are parameterized on the expression language. In the above
-- example, @exp@ can be any type (of kind @* -> *@) that implements the
-- 'EvalExp' and 'CompExp' classes.
--
-- Some examples of using the library are found in the @examples@ directory.

module Language.Embedded.Imperative
  ( module Control.Monad
  , module Data.Int
  , module Data.Word
    -- * Program monad
  , ProgramT
  , Program
  , interpretT
  , interpret
    -- * Imperative instructions
  , RefCMD
  , ArrCMD
  , ControlCMD
  , FileCMD
  , CallCMD
    -- * Types of Printf arguments
  , PrintfArg
    -- * Composing instruction sets
  , (:+:)
  , (:<:)
  , IExp
    -- * Interpreting expressions
  , VarPred
  , EvalExp
  , CompExp
    -- * Front end
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
import Language.Embedded.Imperative.Backend.C ()

