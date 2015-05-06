-- | Deep embedding of imperative programs with code generation. This is the
-- main module for users who want to write imperative programs.

module Language.Embedded.Imperative
  ( module Control.Monad
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
    -- * Composing instruction sets
  , (:+:)
  , (:<:)
  , IExp
    -- * Interpreting expressions
  , VarPred
  , EvalExp
  , CompExp
    -- * Front end
  , module Language.Embedded.Imperative.Types
  , module Language.Embedded.Imperative.Frontend
    -- * C code generation
  , compile
  , icompile
  ) where



import Control.Monad

import Control.Monad.Operational.Compositional
import Data.TypePredicates
import Language.Embedded.Expression
import Language.Embedded.Imperative.CMD
import Language.Embedded.Imperative.Types
import Language.Embedded.Imperative.Frontend
import Language.Embedded.Backend.C

