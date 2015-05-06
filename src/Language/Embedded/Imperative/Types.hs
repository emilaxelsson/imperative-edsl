-- | Re-exports of extra types used by imperative instructions

module Language.Embedded.Imperative.Types
  ( Any
  , Ref (..)
  , Arr (..)
  , IO.IOMode (..)
  , Handle (..)
  , Scannable (..)
  , FunArg (..)
  ) where

import qualified System.IO as IO

import Data.TypePredicates
import Language.Embedded.Imperative.CMD

