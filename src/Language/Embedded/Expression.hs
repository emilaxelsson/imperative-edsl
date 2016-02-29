-- | Interface to pure expressions

module Language.Embedded.Expression where



import Data.Constraint



-- | Variable identifier
type VarId = String

-- | Expressions that support injection of values and named variables
class FreeExp exp
  where
    -- | Constraint on the types of values and variables in an expression
    -- language
    type VarPred exp :: * -> Constraint

    -- | Construct a value expression
    valExp :: VarPred exp a => a -> exp a

    -- | Construct a named variable expression
    varExp :: VarPred exp a => VarId -> exp a

-- | Expressions that support evaluation
class FreeExp exp => EvalExp exp
    -- The super class is motivated by the fact that evaluation of functions
    -- `exp a -> exp b` can be done by constructing an argument using `valExp`.
  where
    -- | Evaluation of a closed expression
    evalExp :: exp a -> a

