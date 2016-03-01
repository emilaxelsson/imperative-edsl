{-# LANGUAGE QuasiQuotes #-}

module Language.Embedded.Backend.C.Expression where



import Data.Proxy

import Language.C.Monad
import Language.C.Quote.C
import Language.C.Syntax (Exp,Type)
import qualified Language.C.Syntax as C

import Language.Embedded.Expression



-- | General interface for compiling expressions
class FreeExp exp => CompExp exp where
  -- The super class is motivated by the fact that compilation of functions
  -- `exp a -> exp b` can be done by constructing an argument using `varExp`.

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

-- | Alternative to 'compType' which receives the expression type from a
-- \"command\" type
compTypeFromCMD :: forall m exp a cmd (prog :: * -> *) x proxy
    .  (MonadC m, CompExp exp, VarPred exp a)
    => cmd exp prog x -> proxy a -> m C.Type
compTypeFromCMD _ = compType (Proxy :: Proxy exp)

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

