{-# LANGUAGE QuasiQuotes #-}

-- | Simple expression type for use in imperative EDSLs

module Language.Embedded.Expr where



import Data.Typeable

import Language.C.Quote.C
import qualified Language.C.Syntax as C

import Language.C.Monad
import Language.Embedded.Imperative
import Language.Embedded.Interpretation



data Expr a
  where
    Val :: Show a => a -> Expr a
    Var :: VarId -> Expr a

    Add :: Num a               => Expr a -> Expr a -> Expr a
    Sub :: Num a               => Expr a -> Expr a -> Expr a
    Mul :: Num a               => Expr a -> Expr a -> Expr a
    Div :: Fractional a        => Expr a -> Expr a -> Expr a
    Exp :: Floating a          => Expr a -> Expr a -> Expr a
    Sin :: Floating a          => Expr a -> Expr a
    Mod :: Integral a          => Expr a -> Expr a -> Expr a
    I2N :: (Integral a, Num b) => Expr a -> Expr b

    Not :: Expr Bool -> Expr Bool
    And :: Expr Bool -> Expr Bool -> Expr Bool
    Or  :: Expr Bool -> Expr Bool -> Expr Bool

    Eq  :: Eq a  => Expr a -> Expr a -> Expr Bool
    LEq :: Ord a => Expr a -> Expr a -> Expr Bool
  deriving Typeable

type instance VarPred Expr = Show

evalExpr :: Expr a -> a
evalExpr (Val a)   = a
evalExpr (Add a b) = evalExpr a + evalExpr b
evalExpr (Sub a b) = evalExpr a - evalExpr b
evalExpr (Mul a b) = evalExpr a * evalExpr b
evalExpr (Div a b) = evalExpr a / evalExpr b
evalExpr (Mod a b) = evalExpr a `mod` evalExpr b
evalExpr (Sin a)   = sin $ evalExpr a
evalExpr (I2N a)   = fromInteger $ fromIntegral $ evalExpr a
evalExpr (Not   a) = not $ evalExpr a
evalExpr (And a b) = evalExpr a && evalExpr b
evalExpr (Or  a b) = evalExpr a || evalExpr b
evalExpr (Eq  a b) = evalExpr a == evalExpr b
evalExpr (LEq a b) = evalExpr a <= evalExpr b

instance EvalExp Expr
  where
    litExp  = Val
    evalExp = evalExpr

compExpr :: (MonadC m) => Expr a -> m C.Exp
compExpr (Var v) = return [cexp| $id:v |]
compExpr (Val v) = case show v of
    "True"  -> addInclude "<stdbool.h>" >> return [cexp| true |]
    "False" -> addInclude "<stdbool.h>" >> return [cexp| false |]
    v'      -> return [cexp| $id:v' |]
compExpr (Add a b) = do
  a' <- compExpr a
  b' <- compExpr b
  return [cexp| $a' + $b' |]
compExpr (Sub a b) = do
  a' <- compExpr a
  b' <- compExpr b
  return [cexp| $a' - $b' |]
compExpr (Mul a b) = do
  a' <- compExpr a
  b' <- compExpr b
  return [cexp| $a' * $b' |]
compExpr (Div a b) = do
  a' <- compExpr a
  b' <- compExpr b
  return [cexp| $a' / $b' |]
compExpr (Exp a b) = do
  a' <- compExpr a
  b' <- compExpr b
  return [cexp| $a' ^ $b' |]
compExpr (Sin a)   = do
  a' <- compExpr a
  return [cexp| sin( $a' ) |]
compExpr (Mod a b) = do
  a' <- compExpr a
  b' <- compExpr b
  return [cexp| $a' % $b'|]
compExpr (I2N a) = do
  a' <- compExpr a
  return [cexp| $a' |]
compExpr (Not  a)  = do
  a' <- compExpr a
  return [cexp| ! $a' |]
compExpr (And a b) = do
  a' <- compExpr a
  b' <- compExpr b
  return [cexp| ($a' && $b') |]
compExpr (Or a b)  = do
  a' <- compExpr a
  b' <- compExpr b
  return [cexp| ($a' || $b') |]
compExpr (Eq a b)  = do
  a' <- compExpr a
  b' <- compExpr b
  return [cexp| $a' == $b' |]
compExpr (LEq a b) = do
  a' <- compExpr a
  b' <- compExpr b
  return [cexp| $a' <= $b' |]

instance CompExp Expr
  where
    varExp  = Var
    compExp = compExpr

instance (Show a, Num a, Eq a) => Num (Expr a)
  where
    fromInteger   = Val . fromInteger
    Val a + Val b = Val (a+b)
    Val 0 + b     = b
    a     + Val 0 = a
    a     + b     = Add a b
    Val a - Val b = Val (a-b)
    Val 0 - b     = b
    a     - Val 0 = a
    a     - b     = Sub a b
    Val a * Val b = Val (a*b)
    Val 0 * b     = Val 0
    a     * Val 0 = Val 0
    Val 1 * b     = b
    a     * Val 1 = a
    a     * b     = Mul a b

    abs    = error "abs not implemented for Expr"
    signum = error "signum not implemented for Expr"

instance (Show a, Fractional a, Eq a) => Fractional (Expr a)
  where
    fromRational  = Val . fromRational
    Val a / Val b = Val (a/b)
    a     / b     = Div a b

    recip = error "recip not implemented for Expr"

true, false :: Expr Bool
true  = Val True
false = Val False

