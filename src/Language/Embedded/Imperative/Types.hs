-- | Re-exports of extra types used by imperative front ends. The motivation for
-- this module is to support making specialized front ends (e.g. like
-- "Language.Embedded.Imperative.Frontend" but specialized for a specific
-- instruction set). The types exported here are the parts of the front end that
-- are independent of the instruction set and/or expression language.

module Language.Embedded.Imperative.Types
  ( Any
  , Ref
  , Arr
  , IO.IOMode (..)
  , Handle
  , Scannable (..)
  , FunArg (..)
  ) where
  -- Note: Important not to export the constructors of `Ref`, `Arr` or `Handle`,
  -- since the user is not supposed to inspect such values.

import qualified System.IO as IO

import Data.TypePredicates
import Language.Embedded.Imperative.CMD

