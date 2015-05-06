-- | Exports the general parts of imperative front ends. The motivation for this
-- module is to support making specialized front ends (e.g. like
-- "Language.Embedded.Imperative.Frontend" but specialized for a specific
-- instruction set). These exports are the parts of the front end that are
-- independent of the instruction set and/or expression language.

module Language.Embedded.Imperative.Frontend.General
  ( Any
  , Ref
  , Arr
  , IO.IOMode (..)
  , Handle
  , stdin
  , stdout
  , Formattable
  , FunArg (..)
  , Definition
  ) where
  -- Note: Important not to export the constructors of `Ref`, `Arr` or `Handle`,
  -- since the user is not supposed to inspect such values.

import qualified System.IO as IO

import Data.TypePredicates
import Language.Embedded.Imperative.CMD

import Language.C.Syntax

