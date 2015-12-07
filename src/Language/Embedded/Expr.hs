{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Typed deep embedding of simple C expressions
--
-- This is a subset of C expression that don't require any control structures
-- and can be compiled to a single-line C expression (plus possibly include
-- statements).

module Language.Embedded.Expr where



import Data.Int
import Data.Maybe
import Data.Word
#if __GLASGOW_HASKELL__ < 710
import Data.Monoid
#endif
import Data.Typeable

#if MIN_VERSION_syntactic(3,0,0)
import Language.Syntactic
import Language.Syntactic.Functional (Denotation)
#elif MIN_VERSION_syntactic(2,0,0)
import Data.Syntactic
import Data.Syntactic.Functional (Denotation)
#else
import Language.Syntactic
#endif

import Language.C.Quote.C
import Language.C.Syntax (Type, UnOp (..), BinOp (..), Exp (UnOp, BinOp))

import Language.C.Monad
import Language.Embedded.Expression



--------------------------------------------------------------------------------
-- * Types
--------------------------------------------------------------------------------

-- | Types supported by C
class (Show a, Eq a, Typeable a) => CType a
  where
    cType :: MonadC m => proxy a -> m Type

instance CType Bool   where cType _ = addSystemInclude "stdbool.h" >> return [cty| typename bool     |]
instance CType Int8   where cType _ = addSystemInclude "stdint.h"  >> return [cty| typename int8_t   |]
instance CType Int16  where cType _ = addSystemInclude "stdint.h"  >> return [cty| typename int16_t  |]
instance CType Int32  where cType _ = addSystemInclude "stdint.h"  >> return [cty| typename int32_t  |]
instance CType Int64  where cType _ = addSystemInclude "stdint.h"  >> return [cty| typename int64_t  |]
instance CType Word8  where cType _ = addSystemInclude "stdint.h"  >> return [cty| typename word8_t  |]
instance CType Word16 where cType _ = addSystemInclude "stdint.h"  >> return [cty| typename word16_t |]
instance CType Word32 where cType _ = addSystemInclude "stdint.h"  >> return [cty| typename word32_t |]
instance CType Word64 where cType _ = addSystemInclude "stdint.h"  >> return [cty| typename word64_t |]

instance CType Float  where cType _ = return [cty| float |]
instance CType Double where cType _ = return [cty| double |]



--------------------------------------------------------------------------------
-- * Expressions
--------------------------------------------------------------------------------

-- | Syntactic symbols for C
data Sym sig
  where
    -- Function or literal
#if MIN_VERSION_syntactic(2,0,0)
    Fun  :: Signature sig => String -> Denotation sig -> Sym sig
#else
    Fun  :: String -> Denotation sig -> Sym sig
#endif
    -- Unary operator
    UOp  :: UnOp -> (a -> b) -> Sym (a :-> Full b)
    -- Binary operator
    Op   :: BinOp -> (a -> b -> c) -> Sym (a :-> b :-> Full c)
    -- Type casting (ignored when generating code)
    Cast :: (a -> b) -> Sym (a :-> Full b)
    -- Variable (only for compilation)
    Var  :: String -> Sym (Full a)

data T sig
  where
    T :: CType (DenResult sig) => { unT :: Sym sig } -> T sig

-- | C expression
newtype Expr a = Expr {unExpr :: ASTF T a}

instance Syntactic (Expr a)
  where
    type Domain (Expr a)   = T
    type Internal (Expr a) = a
    desugar = unExpr
    sugar   = Expr

type instance VarPred Expr = CType

evalSym :: Sym sig -> Denotation sig
evalSym (Fun _ a) = a
evalSym (UOp _ f) = f
evalSym (Op  _ f) = f
evalSym (Cast f)  = f
evalSym (Var v)   = error $ "evalExpr: cannot evaluate variable " ++ v

-- | Evaluate an expression
evalExpr :: Expr a -> a
evalExpr (Expr e) = go e
  where
    go :: AST T sig -> Denotation sig
    go (Sym (T s)) = evalSym s
    go (f :$ a)    = go f $ go a

instance EvalExp Expr
  where
    litExp a = Expr $ Sym $ T $ Fun (show a) a
    evalExp  = evalExpr

-- | Compile an expression
compExpr :: forall m a . MonadC m => Expr a -> m Exp
compExpr = simpleMatch (go . unT) . unExpr
  where
    compExpr' :: ASTF T b -> m Exp
    compExpr' = compExpr . Expr

    go :: Sym sig -> Args (AST T) sig -> m Exp
    go (Var v) Nil = return [cexp| $id:v |]
    go (Fun lit _) Nil = case lit of
      "True"  -> addSystemInclude "stdbool.h" >> return [cexp| true |]
      "False" -> addSystemInclude "stdbool.h" >> return [cexp| false |]
      l       -> return [cexp| $id:l |]
    go (Fun fun _) args = do
      as <- sequence $ listArgs compExpr' args
      return [cexp| $id:fun($args:as) |]
    go (UOp op _) (a :* Nil) = do
      a' <- compExpr' a
      return $ UnOp op a' mempty
    go (Op op _) (a :* b :* Nil) = do
      a' <- compExpr' a
      b' <- compExpr' b
      return $ BinOp op a' b' mempty
    go (Cast f) (a :* Nil) = do
      a' <- compExpr' a
      return [cexp| $a' |]

instance CompExp Expr
  where
    varExp = Expr . Sym . T . Var . showVar
      where showVar v = 'v' : show v
    compExp  = compExpr
    compType = cType

-- | One-level constant folding: if all immediate sub-expressions are literals,
-- the expression is reduced to a single literal
constFold :: Expr a -> Expr a
constFold = Expr . match go . unExpr
  where
    go :: T sig -> Args (AST T) sig -> AST T (Full (DenResult sig))
    go (T s) as = res
      where
        e   = appArgs (Sym $ T s) as
        res = if and $ listArgs (isJust . viewLit . Expr) as
                then unExpr $ value $ evalExpr $ Expr e
                else e
  -- Deeper constant folding would require a way to witness `Show` for arbitrary
  -- sub-expressions. This is certainly doable, but seems to complicate things
  -- for not much gain (currently).

-- | Get the value of a literal expression
viewLit :: Expr a -> Maybe a
viewLit (Expr (Sym (T (Fun _ a)))) = Just a
viewLit _ = Nothing



--------------------------------------------------------------------------------
-- * User interface
--------------------------------------------------------------------------------

-- | Construct a literal expression
value :: CType a => a -> Expr a
value a = Expr $ Sym $ T $ Fun (show a) a

true, false :: Expr Bool
true  = value True
false = value False

instance (Num a, CType a) => Num (Expr a)
  where
    fromInteger = value . fromInteger

    a + b
      | Just 0 <- viewLit a = b
      | Just 0 <- viewLit b = a
      | otherwise           = constFold $ sugarSym (T $ Op Add (+)) a b

    a - b
      | Just 0 <- viewLit a = negate b
      | Just 0 <- viewLit b = a
      | otherwise           = constFold $ sugarSym (T $ Op Sub (-)) a b

    a * b
      | Just 0 <- viewLit a = value 0
      | Just 0 <- viewLit b = value 0
      | Just 1 <- viewLit a = b
      | Just 1 <- viewLit b = a
      | otherwise           = constFold $ sugarSym (T $ Op Mul (*)) a b

    negate a = constFold $ sugarSym (T $ UOp Negate negate) a

    abs    = error "abs not implemented for Expr"
    signum = error "signum not implemented for Expr"

instance (Fractional a, CType a) => Fractional (Expr a)
  where
    fromRational = value . fromRational
    a / b = constFold $ sugarSym (T $ Op Div (/)) a b

    recip = error "recip not implemented for Expr"

castAST :: forall a b . Typeable b => ASTF T a -> Maybe (ASTF T b)
castAST a = simpleMatch go a
  where
    go :: (DenResult sig ~ a) => T sig -> Args (AST T) sig -> Maybe (ASTF T b)
    go (T _) _ = cast a

-- | Boolean negation
not_ :: Expr Bool -> Expr Bool
not_ (Expr (nt :$ a))
    | Just (T (UOp Lnot _)) <- prj nt
    , Just a' <- castAST a = Expr a'
not_ a = constFold $ sugarSym (T $ UOp Lnot not) a

-- | Equality
(<==>) :: Eq a => Expr a -> Expr a -> Expr Bool
a <==> b = constFold $ sugarSym (T $ Op Eq (==)) a b

-- | Integral type casting
i2n :: (Integral a, Num b, CType b) => Expr a -> Expr b
i2n a = constFold $ sugarSym (T $ Cast (fromInteger . toInteger)) a










--------------------------------------------------------------------------------
-- * Boilerplate instances
--------------------------------------------------------------------------------

-- These can be derived in Syntactic >= 3.1

#if MIN_VERSION_syntactic(2,0,0)
instance Symbol Sym
  where
    symSig (Fun _ _) = signature
    symSig (UOp _ _) = signature
    symSig (Op _ _)  = signature
    symSig (Cast _)  = signature
    symSig (Var _)   = signature

instance Render Sym
  where
    renderSym (Fun name _) = name
    renderSym (UOp op _)   = show op
    renderSym (Op op _)    = show op
    renderSym (Cast _)     = "cast"
    renderSym (Var v)      = v
    renderArgs = renderArgsSmart

instance Equality Sym
  where
    equal = equalDefault
    hash  = hashDefault

instance StringTree Sym

instance Symbol T where symSig (T s) = symSig s

instance Render T
  where
    renderSym (T s)     = renderSym s
    renderArgs as (T s) = renderArgs as s

instance Equality T
  where
    equal (T s) (T t) = equal s t
    hash (T s)        = hash s

instance StringTree T
  where
    stringTreeSym as (T s) = stringTreeSym as s
#endif

