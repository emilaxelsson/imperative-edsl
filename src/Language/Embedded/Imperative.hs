-- | Deep embedding of imperative programs with code generation. This is the
-- main module for users who want to write imperative programs.

module Language.Embedded.Imperative
  ( module Control.Monad
  , module Data.TypePredicates
  , module Data.ALaCarte
  , module Control.Monad.Operational.Compositional
  , module Language.Embedded.Expression
  , module Language.Embedded.Imperative.Types
  , module Language.Embedded.Imperative.CMD
  , module Language.Embedded.Imperative.Frontend
  , module Language.Embedded.Backend.C
  ) where



import Control.Monad

import Control.Monad.Operational.Compositional (ProgramT, Program, interpretT, interpret, IExp)
import Data.TypePredicates ((:<), Any)
import Data.ALaCarte ((:+:), (:<:))
import Language.Embedded.Expression (VarPred, EvalExp, CompExp)
import Language.Embedded.Imperative.CMD (RefCMD, ArrCMD, ControlCMD, FileCMD, CallCMD)
import Language.Embedded.Imperative.Types
import Language.Embedded.Imperative.Frontend
import Language.Embedded.Backend.C (compile, icompile)

