{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module CExp where



import Data.Int

import Test.Tasty.QuickCheck
import Test.Tasty.TH

import Language.Syntactic (AST (..), DenResult)
import Language.Syntactic.Functional (Denotation)

import Language.Embedded.Imperative
import Language.Embedded.Backend.C
import Language.Embedded.CExp



data NumExp
    = VAR Int
    | INT Int
    | ADD NumExp NumExp
    | SUB NumExp NumExp
    | MUL NumExp NumExp
    | NEG NumExp
  deriving (Eq, Show)

evalNumExp :: Num a => (Int -> a) -> NumExp -> a
evalNumExp env (VAR v)   = env v
evalNumExp env (INT i)   = fromIntegral i
evalNumExp env (ADD a b) = evalNumExp env a + evalNumExp env b
evalNumExp env (SUB a b) = evalNumExp env a - evalNumExp env b
evalNumExp env (MUL a b) = evalNumExp env a * evalNumExp env b
evalNumExp env (NEG a)   = negate (evalNumExp env a)

num2CExp :: (Num a, Ord a, CType a) => NumExp -> CExp a
num2CExp = evalNumExp (\v -> variable ('v' : show v))

-- | Partial function for evaluating 'CExp' produced by 'num2CExp'
evalNumCExp :: forall a
    .  (String -> a)  -- ^ Mapping from variable names to values
    -> CExp a
    -> a
evalNumCExp env = go . unCExp
  where
    go :: (a ~ DenResult sig) => AST T sig -> Denotation sig
    go (Sym (T (Var v)))                = env v
    go (Sym (T s))                      = evalSym s
    go (Sym (T s@(UOp UnNeg)) :$ a)     = evalSym s $ go a
    go (Sym (T s@(Op BiAdd)) :$ a :$ b) = evalSym s (go a) (go b)
    go (Sym (T s@(Op BiSub)) :$ a :$ b) = evalSym s (go a) (go b)
    go (Sym (T s@(Op BiMul)) :$ a :$ b) = evalSym s (go a) (go b)

genNumExp :: Gen NumExp
genNumExp = sized go
  where
    go s = frequency
        [ -- Variable
          (1, do v <- choose (0,4)
                 return $ VAR v
          )
          -- Literal
        , (1, fmap INT $ elements [-100 .. 100])
        , (s, binOp ADD)
        , (s, binOp SUB)
        , (s, binOp MUL)
        , (s, unOp NEG)
        ]
      where
        binOp op = liftM2 op (go (s `div` 2)) (go (s `div` 2))
        unOp op  = liftM op (go (s-1))

instance Arbitrary NumExp
  where
    arbitrary = genNumExp

    shrink (ADD a b) = a : b : [ADD a' b | a' <- shrink a] ++ [ADD a b' | b' <- shrink b]
    shrink (SUB a b) = a : b : [SUB a' b | a' <- shrink a] ++ [SUB a b' | b' <- shrink b]
    shrink (MUL a b) = a : b : [MUL a' b | a' <- shrink a] ++ [MUL a b' | b' <- shrink b]
    shrink (NEG a)   = a : [NEG a' | a' <- shrink a]
    shrink _         = []

-- Test that numeric expressions are simplified correctly
prop_numExp :: (a ~ Int32) => (a,a,a,a,a) -> NumExp -> Bool
prop_numExp (a,b,c,d,e) numExp =
    evalNumExp env1 numExp == evalNumCExp env2 (num2CExp numExp)
  where
    env1 v       = [a,b,c,d,e] !! v
    env2 ('v':v) = [a,b,c,d,e] !! read v

-- Test that inexact numeric expressions are handled correctly
--
-- This property fails if one changes `isExact` to `const True`
prop_numExp_inexact :: (a ~ Float) => (a,a,a,a,a) -> NumExp -> Bool
prop_numExp_inexact (a,b,c,d,e) numExp =
    evalNumExp env1 numExp == evalNumCExp env2 (num2CExp numExp)
  where
    env1 v       = [a,b,c,d,e] !! v
    env2 ('v':v) = [a,b,c,d,e] !! read v

main = $defaultMainGenerator

