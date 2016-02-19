{-# LANGUAGE QuasiQuotes #-}

-- | Interface for evaluation and compilation of pure expressions
module Language.Embedded.Expression
  ( VarId
  , VarPred
  , EvalExp(..)
  , CompExp(..)
  , proxyArg
  , freshVar
  , freshVar_
  )
  where

import Data.Proxy
import Data.Constraint
import Language.C.Monad
import Language.C.Quote.C
import Language.C.Syntax (Exp,Type)
import qualified Language.C.Syntax as C



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
class CompExp exp where
    -- | Variable expressions
    varExp  :: VarPred exp a => VarId -> exp a

    -- | Compilation of expressions
    --
    -- /NOTE: It is assumed that free variables in the expression are rendered as @vIII@, where/
    -- /      @III@ is the variable identifier./
    compExp :: (MonadC m) => exp a -> m Exp

    -- | Extract expression type
    compType :: (MonadC m, VarPred exp a) => proxy1 exp -> proxy2 a -> m Type

-- | Remove one layer of a nested proxy
proxyArg :: proxy1 (proxy2 a) -> Proxy a
proxyArg _ = Proxy

-- | Variable identifier
type VarId = Integer

-- | Create and declare a fresh variable and return its name
freshVar :: forall exp m a. (CompExp exp, VarPred exp a, MonadC m) => m (exp a, C.Id)
freshVar = do
    v <- fmap varExp freshId
    t <- compType (Proxy :: Proxy exp) (Proxy :: Proxy a)
    C.Var n _ <- compExp v
    touchVar n
    case t of
      C.Type _ C.Ptr{} _ -> addLocal [cdecl| $ty:t $id:n = NULL; |]
      _                  -> addLocal [cdecl| $ty:t $id:n; |]
    return (v,n)

-- | Create and declare a fresh variable
freshVar_ :: forall exp m a. (CompExp exp, VarPred exp a, MonadC m) => m (exp a)
freshVar_ = fst `fmap` freshVar
