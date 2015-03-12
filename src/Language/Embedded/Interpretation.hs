{-# LANGUAGE UndecidableInstances #-}

-- | Interpretation of @Language.Embedded@ programs
module Language.Embedded.Interpretation
  ( VarId
  , VarPred
  , EvalExp(..)
  , CompExp(..)
  , Any
  , (:/\:)
  )
  where

import Data.Constraint
import Language.C.Monad (MonadC)
import Language.C.Syntax


----------------------------------------------------------------------------------------------------
-- * Interpretation of expressions
----------------------------------------------------------------------------------------------------

-- | Constraint on the types of variables in a given expression language
type family VarPred (exp :: * -> *) :: * -> Constraint

-- | General interface for evaluating expressions
class EvalExp exp
  where
    -- | Literal expressions
    litExp  :: VarPred exp a => a -> exp a

    -- | Evaluation of (closed) expressions
    evalExp :: exp a -> a

-- | General interface for compiling expressions
class CompExp exp
  where
    -- | Variable expressions
    varExp  :: VarPred exp a => VarId -> exp a

    -- | Compilation of expressions
    compExp :: (MonadC m) => exp a -> m Exp

-- | Variable identifier
type VarId = String

-- | Universal predicate
class    Any a
instance Any a

-- | Predicate conjunction
class    (p1 a, p2 a) => (p1 :/\: p2) a
instance (p1 a, p2 a) => (p1 :/\: p2) a



