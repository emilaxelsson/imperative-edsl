-- | Deep embedding of imperative programs with code generation

module Language.Embedded.Imperative
  ( module Data.TypePredicates
  , module Control.Monad.Operational.Compositional
  , module Language.Embedded.Expression
  , module Language.Embedded.Imperative.Types
  , module Language.Embedded.Imperative.CMD
  , module Language.Embedded.Imperative.Frontend
  ) where



import Data.TypePredicates
import Control.Monad.Operational.Compositional
import Language.Embedded.Expression
import Language.Embedded.Imperative.CMD (RefCMD, ArrCMD, ControlCMD, FileCMD, CallCMD)
import Language.Embedded.Imperative.Types
import Language.Embedded.Imperative.Frontend
import Language.Embedded.Backend.C ()

