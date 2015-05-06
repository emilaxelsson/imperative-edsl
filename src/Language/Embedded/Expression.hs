{-# LANGUAGE QuasiQuotes #-}

-- | Interface for evaluation and compilation of pure expressions
module Language.Embedded.Expression
  ( VarId
  , VarPred
  , EvalExp(..)
  , CompExp(..)
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
    compType :: forall m a
             .  (MonadC m, VarPred exp a)
             => exp a -> m Type
    compType _ = compTypeP (Proxy :: Proxy (exp a))
    {-# INLINE compType #-}

    -- | Extract expression type
    compTypeP :: forall proxy m a
              .  (MonadC m, VarPred exp a)
              => proxy (exp a) -> m Type
    compTypeP _ = compTypePP (Proxy :: Proxy exp) (Proxy :: Proxy a)
    {-# INLINE compTypeP #-}

    -- | Extract expression type
    compTypePP :: forall proxy1 proxy2 m a
               .  (MonadC m, VarPred exp a)
               => proxy1 exp -> proxy2 a -> m Type
    compTypePP _ _ = compTypePP2 (Proxy :: Proxy exp) (Proxy :: Proxy (Proxy a))
    {-# INLINE compTypePP #-}

    -- | Extract expression type
    compTypePP2 :: forall proxy proxy1 proxy2 m a
                .  (MonadC m, VarPred exp a)
                => proxy exp -> proxy1 (proxy2 a) -> m Type
    compTypePP2 _ _ = compType (undefined :: exp a)
    {-# INLINE compTypePP2 #-}

    {-# MINIMAL varExp , compExp , (compType | compTypeP | compTypePP | compTypePP2 ) #-}

-- | Variable identifier
type VarId = Integer

-- | Create and declare a fresh variable and return its name
freshVar :: forall exp m a. (CompExp exp, VarPred exp a, MonadC m) => m (exp a, C.Id)
freshVar = do
    v <- fmap varExp freshId
    t <- compTypeP (Proxy :: Proxy (exp a))
    C.Var n _ <- compExp v
    touchVar n
    case t of
      C.Type _ C.Ptr{} _ -> addLocal [cdecl| $ty:t $id:n = NULL; |]
      _                  -> addLocal [cdecl| $ty:t $id:n; |]
    return (v,n)

-- | Create and declare a fresh variable
freshVar_ :: forall exp m a. (CompExp exp, VarPred exp a, MonadC m) => m (exp a)
freshVar_ = fst `fmap` freshVar
