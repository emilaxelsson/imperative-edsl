-- | Deep embedding of imperative programs with code generation

module Language.Embedded.Imperative
  ( module Control.Monad
  , module Data.TypePredicates
  , module Data.ALaCarte
  , module Control.Monad.Operational.Compositional
  , module Language.Embedded.Expression
  , module Language.Embedded.Imperative.Types
  , module Language.Embedded.Imperative.CMD
  , module Language.Embedded.Imperative.Frontend
  ) where



import Control.Monad

import Control.Monad.Operational.Compositional (ProgramT, Program, interpretT, interpret)
import Data.TypePredicates (Any)
import Data.ALaCarte ((:+:), (:<:))
import Language.Embedded.Expression (VarPred, EvalExp, CompExp)
import Language.Embedded.Imperative.CMD (RefCMD, ArrCMD, ControlCMD, FileCMD, CallCMD)
import Language.Embedded.Imperative.Types
import Language.Embedded.Imperative.Frontend
import Language.Embedded.Backend.C ()

