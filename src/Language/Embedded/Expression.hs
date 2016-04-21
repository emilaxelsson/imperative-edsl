{-# LANGUAGE CPP #-}

-- | Interface to pure expressions

module Language.Embedded.Expression where



import Data.Typeable

import GHC.Prim (Constraint)

import Language.C.Quote.C (ToIdent (..))



-- | Variable identifier
type VarId = String

-- | Expressions that support injection of constants and named variables
class FreeExp exp
  where
    -- | Constraint on the types of constants and variables in an expression
    -- language
    type FreePred exp :: * -> Constraint

    -- | Inject a constant value
    constExp :: FreePred exp a => a -> exp a

    -- | Inject a named variable
    varExp :: FreePred exp a => VarId -> exp a

-- | Value
data Val a
    = ValComp VarId  -- ^ Symbolic value
    | ValRun a       -- ^ Concrete value
  deriving Typeable

instance ToIdent (Val a) where toIdent (ValComp r) = toIdent r

-- | Convert a value to an expression
valToExp :: (FreeExp exp, FreePred exp a) => Val a -> exp a
valToExp (ValComp v) = varExp v
valToExp (ValRun a)  = constExp a

-- | Expressions that support evaluation
class FreeExp exp => EvalExp exp
    -- The super class is motivated by the fact that evaluation of functions
    -- `exp a -> exp b` can be done by constructing an argument using
    -- `constExp`.
  where
    -- | Evaluation of a closed expression
    evalExp :: exp a -> a

