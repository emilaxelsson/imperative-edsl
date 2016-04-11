{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Embedded.Signature where

import Data.Proxy

import Language.C.Monad
import Language.Embedded.Expression
import Language.Embedded.Backend.C.Expression

import Language.C.Quote.C


-- * Language

-- | Signature annotations
data Ann exp a where
  Empty  :: Ann exp a
  Native :: (FreePred exp a) => exp len -> Ann exp [a]
  Named  :: String -> Ann exp a

-- | Signatures
data Signature exp pred a where
  Ret    :: pred a => String -> exp a -> Signature exp pred a
  Ptr    :: pred a => String -> exp a -> Signature exp pred a
  Lam    :: pred a => Ann exp a -> (Val a -> Signature exp pred b)
         -> Signature exp pred (a -> b)


-- * Combinators

lam :: (pred a, FreeExp exp, FreePred exp a)
    => (exp a -> Signature exp pred b) -> Signature exp pred (a -> b)
lam f = Lam Empty $ \x -> f (valToExp x)

name :: (pred a, FreeExp exp, FreePred exp a)
     => String -> (exp a -> Signature exp pred b) -> Signature exp pred (a -> b)
name s f = Lam (Named s) $ \x -> f (valToExp x)

ret,ptr :: (pred a)
        => String -> exp a -> Signature exp pred a
ret = Ret
ptr = Ptr

arg :: (pred a, FreeExp exp, FreePred exp a)
    => Ann exp a
    -> (exp a -> exp b)
    -> (exp b -> Signature exp pred c)
    -> Signature exp pred (a -> c)
arg s g f = Lam s $ \x -> f $ g $ valToExp x



-- * Compilation

-- | Compile a function @Signature@ to C code
translateFunction :: forall m exp a. (MonadC m, CompExp exp)
                  => Signature exp CType a -> m ()
translateFunction sig = go sig (return ())
  where
    go :: Signature exp CType d -> m () -> m ()
    go (Ret n a) prelude = do
      t <- cType a
      inFunctionTy t n $ do
        prelude
        e <- compExp a
        addStm [cstm| return $e; |]
    go (Ptr n a) prelude = do
      t <- cType a
      inFunction n $ do
        prelude
        e <- compExp a
        addParam [cparam| $ty:t *out |]
        addStm [cstm| *out = $e; |]
    go fun@(Lam Empty f) prelude = do
      t <- cType (argProxy fun)
      v <- freshVar (Proxy :: Proxy CType)
      go (f v) $ prelude >> addParam [cparam| $ty:t $id:v |]
    go fun@(Lam n@(Native l) f) prelude = do
      t <- cType n
      i <- freshId
      let vi = 'v' : show i
      let w = ValComp vi
      let n = vi ++ "_buf"
      withAlias i ('&':vi) $ go (f w) $ do
        prelude
        len <- compExp l
        addLocal [cdecl| struct array $id:vi = { .buffer = $id:n
                                               , .length=$len
                                               , .elemSize=sizeof($ty:t)
                                               , .bytes=sizeof($ty:t)*$len
                                               }; |]
        addParam [cparam| $ty:t * $id:n |]
    go fun@(Lam (Named s) f) prelude = do
      t <- cType (argProxy fun)
      i <- freshId
      let w = ValComp ('v' : show i)
      withAlias i s $ go (f w) $ prelude >> addParam [cparam| $ty:t $id:s |]

argProxy :: Signature exp pred (b -> c) -> Proxy b
argProxy _ = Proxy

