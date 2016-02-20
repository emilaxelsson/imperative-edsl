{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Typed deep embedding of simple C expressions
--
-- This is a subset of C expressions that only have simple non-compound and
-- non-pointed types, and that don't contain any control structures.
--
-- (Of course, nothing stops one from translating 'CExp' to something other than
-- C, but its constructors and set of supported types is inspired by C.)

module Language.Embedded.CExp where



import Data.Array
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
import Language.Syntactic.TH
#else
import Language.Syntactic
#endif

#if MIN_VERSION_syntactic(3,0,0)
import Data.TypeRep hiding (Typeable, gcast)
import Data.TypeRep.TH
import Data.TypeRep.Types.Basic
import Data.TypeRep.Types.Tuple
import Data.TypeRep.Types.IntWord
#endif

import Language.C.Quote.C
import Language.C.Syntax (Type, UnOp (..), BinOp (..), Exp (UnOp, BinOp))
import qualified Language.C.Syntax as C

import Language.C.Monad
import Language.Embedded.Expression
import Language.Embedded.Backend.C.Expression
import Language.Embedded.Imperative.CMD (IArr (..))



--------------------------------------------------------------------------------
-- * Types
--------------------------------------------------------------------------------

instance ToExp Int8   where toExp = toExp . toInteger
instance ToExp Int16  where toExp = toExp . toInteger
instance ToExp Int32  where toExp = toExp . toInteger
instance ToExp Int64  where toExp = toExp . toInteger
instance ToExp Word8  where toExp = toExp . toInteger
instance ToExp Word16 where toExp = toExp . toInteger
instance ToExp Word32 where toExp = toExp . toInteger
instance ToExp Word64 where toExp = toExp . toInteger

-- | Types supported by C
class (Show a, Eq a, Typeable a) => CType a
  where
    cType :: MonadC m => proxy a -> m Type

    cLit         :: MonadC m => a -> m Exp
    default cLit :: (ToExp a, MonadC m) => a -> m Exp
    cLit = return . flip toExp mempty

instance CType Bool
  where
    cType _ = do
        addSystemInclude "stdbool.h"
        return [cty| typename bool |]
    cLit b = do
        addSystemInclude "stdbool.h"
        return $ if b then [cexp| true |] else [cexp| false |]

instance CType Int8   where cType _ = addSystemInclude "stdint.h"  >> return [cty| typename int8_t   |]
instance CType Int16  where cType _ = addSystemInclude "stdint.h"  >> return [cty| typename int16_t  |]
instance CType Int32  where cType _ = addSystemInclude "stdint.h"  >> return [cty| typename int32_t  |]
instance CType Int64  where cType _ = addSystemInclude "stdint.h"  >> return [cty| typename int64_t  |]
instance CType Word8  where cType _ = addSystemInclude "stdint.h"  >> return [cty| typename uint8_t  |]
instance CType Word16 where cType _ = addSystemInclude "stdint.h"  >> return [cty| typename uint16_t |]
instance CType Word32 where cType _ = addSystemInclude "stdint.h"  >> return [cty| typename uint32_t |]
instance CType Word64 where cType _ = addSystemInclude "stdint.h"  >> return [cty| typename uint64_t |]

instance CType Float  where cType _ = return [cty| float |]
instance CType Double where cType _ = return [cty| double |]

#if MIN_VERSION_syntactic(3,0,0)
instance ShowClass CType where showClass _ = "CType"

pCType :: Proxy CType
pCType = Proxy

deriveWitness ''CType ''BoolType
deriveWitness ''CType ''FloatType
deriveWitness ''CType ''DoubleType
deriveWitness ''CType ''IntWordType

derivePWitness ''CType ''BoolType
derivePWitness ''CType ''FloatType
derivePWitness ''CType ''DoubleType
derivePWitness ''CType ''IntWordType

instance PWitness CType CharType t
instance PWitness CType ListType t
instance PWitness CType TupleType t
instance PWitness CType FunType t
#endif

-- | Return whether the type of the expression is a floating-point numeric type
isFloat :: forall a . CType a => CExp a -> Bool
isFloat a = t == typeOf (undefined :: Float) || t == typeOf (undefined :: Double)
  where
    t = typeOf (undefined :: a)

-- | Return whether the type of the expression is a non-floating-point type
isExact :: CType a => CExp a -> Bool
isExact = not . isFloat

-- | Return whether the type of the expression is a non-floating-point type
isExact' :: CType a => ASTF T a -> Bool
isExact' = isExact . CExp



--------------------------------------------------------------------------------
-- * Expressions
--------------------------------------------------------------------------------

data Unary a
  where
    UnNeg :: Num a => Unary (a -> a)
    UnNot :: Unary (Bool -> Bool)

evalUnary :: Unary a -> a
evalUnary UnNeg = negate
evalUnary UnNot = not

unaryOp :: Unary a -> UnOp
unaryOp UnNeg = Negate
unaryOp UnNot = Lnot

data Binary a
  where
    BiAdd  :: Num a            => Binary (a -> a -> a)
    BiSub  :: Num a            => Binary (a -> a -> a)
    BiMul  :: Num a            => Binary (a -> a -> a)
    BiDiv  :: Fractional a     => Binary (a -> a -> a)
    BiQuot :: Integral a       => Binary (a -> a -> a)
    BiRem  :: Integral a       => Binary (a -> a -> a)
    BiAnd  ::                     Binary (Bool -> Bool -> Bool)
    BiOr   ::                     Binary (Bool -> Bool -> Bool)
    BiEq   :: CType a          => Binary (a -> a -> Bool)
    BiNEq  :: CType a          => Binary (a -> a -> Bool)
    BiLt   :: (Ord a, CType a) => Binary (a -> a -> Bool)
    BiGt   :: (Ord a, CType a) => Binary (a -> a -> Bool)
    BiLe   :: (Ord a, CType a) => Binary (a -> a -> Bool)
    BiGe   :: (Ord a, CType a) => Binary (a -> a -> Bool)

evalBinary :: Binary a -> a
evalBinary BiAdd  = (+)
evalBinary BiSub  = (-)
evalBinary BiMul  = (*)
evalBinary BiDiv  = (/)
evalBinary BiQuot = quot
evalBinary BiRem  = rem
evalBinary BiAnd  = (&&)
evalBinary BiOr   = (||)
evalBinary BiEq   = (==)
evalBinary BiNEq  = (/=)
evalBinary BiLt   = (<)
evalBinary BiGt   = (>)
evalBinary BiLe   = (<=)
evalBinary BiGe   = (>=)

binaryOp :: Binary a -> BinOp
binaryOp BiAdd  = Add
binaryOp BiSub  = Sub
binaryOp BiMul  = Mul
binaryOp BiDiv  = Div
binaryOp BiQuot = Div
binaryOp BiRem  = Mod
binaryOp BiAnd  = Land
binaryOp BiOr   = Lor
binaryOp BiEq   = Eq
binaryOp BiNEq  = Ne
binaryOp BiLt   = Lt
binaryOp BiGt   = Gt
binaryOp BiLe   = Le
binaryOp BiGe   = Ge

type SupportCode = forall m . MonadC m => m ()

-- | Syntactic symbols for C
data Sym sig
  where
    -- Literal
    Lit   :: String -> a -> Sym (Full a)
    -- Predefined constant
    Const :: SupportCode -> String -> a -> Sym (Full a)
    -- Function call
    Fun   ::
#if MIN_VERSION_syntactic(3,0,0)
             Signature sig =>
#endif
             SupportCode -> String -> Denotation sig -> Sym sig
    -- Unary operator
    UOp   :: Unary (a -> b) -> Sym (a :-> Full b)
    -- Binary operator
    Op    :: Binary (a -> b -> c) -> Sym (a :-> b :-> Full c)
    -- Type casting (ignored when generating code)
    Cast  :: (a -> b) -> Sym (a :-> Full b)
    -- Conditional
    Cond  :: Sym (Bool :-> a :-> a :-> Full a)
    -- Variable (only for compilation)
    Var   :: String -> Sym (Full a)
    -- Unsafe array indexing
    ArrIx :: (Integral i, Ix i) => IArr i a -> Sym (i :-> Full a)

data T sig
  where
    T :: CType (DenResult sig) => { unT :: Sym sig } -> T sig

-- | C expression
newtype CExp a = CExp {unCExp :: ASTF T a}

instance Syntactic (CExp a)
  where
    type Domain (CExp a)   = T
    type Internal (CExp a) = a
    desugar = unCExp
    sugar   = CExp

evalSym :: Sym sig -> Denotation sig
evalSym (Lit _ a)     = a
evalSym (Const _ _ a) = a
evalSym (Fun _ _ f)   = f
evalSym (UOp uop)     = evalUnary uop
evalSym (Op bop)      = evalBinary bop
evalSym (Cast f)      = f
evalSym Cond          = \c t f -> if c then t else f
evalSym (ArrIx (IArrEval arr)) = \i ->
    if i<l || i>h
      then error $ "index "
                ++ show (toInteger i)
                ++ " out of bounds "
                ++ show (toInteger l, toInteger h)
      else arr!i
  where
    (l,h) = bounds arr
evalSym (Var v) = error $ "evalCExp: cannot evaluate variable " ++ v

-- | Evaluate an expression
evalCExp :: CExp a -> a
evalCExp (CExp e) = go e
  where
    go :: AST T sig -> Denotation sig
    go (Sym (T s)) = evalSym s
    go (f :$ a)    = go f $ go a

instance FreeExp CExp
  where
    type VarPred CExp = CType
    valExp a = CExp $ Sym $ T $ Lit (show a) a
    varExp = CExp . Sym . T . Var . showVar
      where showVar v = 'v' : show v

instance EvalExp CExp where evalExp = evalCExp

-- | Compile an expression
compCExp :: forall m a . MonadC m => CExp a -> m Exp
compCExp = simpleMatch (\(T s) -> go s) . unCExp
  where
    compCExp' :: ASTF T b -> m Exp
    compCExp' = compCExp . CExp

    typeOfSym :: forall sig m . MonadC m =>
        CType (DenResult sig) => Sym sig -> m Type
    typeOfSym _ = cType (Proxy :: Proxy (DenResult sig))

    go :: CType (DenResult sig) => Sym sig -> Args (AST T) sig -> m Exp
    go (Var v) Nil   = return [cexp| $id:v |]
    go (Lit _ a) Nil = cLit a
    go (Const code const _) Nil = do
      code
      return [cexp| $id:const |]
    go (Fun code fun _) args = do
      code
      as <- sequence $ listArgs compCExp' args
      return [cexp| $id:fun($args:as) |]
    go (UOp uop) (a :* Nil) = do
      a' <- compCExp' a
      return $ UnOp (unaryOp uop) a' mempty
    go (Op bop) (a :* b :* Nil) = do
      a' <- compCExp' a
      b' <- compCExp' b
      return $ BinOp (binaryOp bop) a' b' mempty
    go s@(Cast f) (a :* Nil) = do
      a' <- compCExp' a
      t <- typeOfSym s
      if t == [cty|typename bool|] || t == [cty|float|] || t == [cty|double|]
        then return [cexp|($ty:t) $a'|]
        else return [cexp| $a' |]
          -- Explicit casting is usually not needed. The reason for doing it for
          -- floating-point types is that
          --
          --     printf("%f",i);
          --
          -- gives an error if `i` is an integer. I'm not sure if there's ever a
          -- need to use an explicit when going to an integer type.
    go Cond (c :* t :* f :* Nil) = do
      c' <- compCExp' c
      t' <- compCExp' t
      f' <- compCExp' f
      return $ C.Cond c' t' f' mempty
    go (ArrIx arr) (i :* Nil) = do
      i' <- compCExp' i
      return [cexp| $id:arr[$i'] |]

instance CompExp CExp
  where
    compExp  = compCExp
    compType _ p = cType p

-- | One-level constant folding: if all immediate sub-expressions are literals,
-- the expression is reduced to a single literal
constFold :: CExp a -> CExp a
constFold = CExp . match go . unCExp
  where
    go :: T sig -> Args (AST T) sig -> AST T (Full (DenResult sig))
    go (T s) as = res
      where
        e   = appArgs (Sym $ T s) as
        res = if and $ listArgs (isJust . viewLit . CExp) as
                then unCExp $ value $ evalCExp $ CExp e
                else e
  -- Deeper constant folding would require a way to witness `Show` for arbitrary
  -- sub-expressions. This is certainly doable, but seems to complicate things
  -- for not much gain (currently).

castAST :: forall a b . Typeable b => ASTF T a -> Maybe (ASTF T b)
castAST a = simpleMatch go a
  where
    go :: (DenResult sig ~ a) => T sig -> Args (AST T) sig -> Maybe (ASTF T b)
    go (T _) _ = gcast a

-- | Get the value of a literal expression
viewLit :: CExp a -> Maybe a
viewLit (CExp (Sym (T (Lit _ a)))) = Just a
viewLit _ = Nothing

pattern LitP a      <- CExp (Sym (T (Lit _ a)))
pattern LitP' a     <- Sym (T (Lit _ a))
pattern NonLitP     <- (viewLit -> Nothing)
pattern NonLitP'    <- (CExp -> (viewLit -> Nothing))
pattern OpP op a b  <- CExp (Sym (T (Op op)) :$ a :$ b)
pattern OpP' op a b <- Sym (T (Op op)) :$ a :$ b
pattern UOpP op a   <- CExp (Sym (T (UOp op)) :$ a)
pattern UOpP' op a  <- Sym (T (UOp op)) :$ a



--------------------------------------------------------------------------------
-- * User interface
--------------------------------------------------------------------------------

-- | Construct a literal expression
value :: CType a => a -> CExp a
value a = CExp $ Sym $ T $ Lit (show a) a

-- | Predefined constant
constant :: CType a
    => SupportCode  -- ^ Supporting C code
    -> String       -- ^ Name of constant
    -> a            -- ^ Value of constant
    -> CExp a
constant code const val = CExp $ Sym $ T $ Const code const val

-- | Create a named variable
variable :: CType a => String -> CExp a
variable = CExp . Sym . T . Var

true, false :: CExp Bool
true  = constant (addInclude "<stdbool.h>") "true" True
false = constant (addInclude "<stdbool.h>") "false" False

instance (Num a, Ord a, CType a) => Num (CExp a)
  where
    fromInteger = value . fromInteger

    LitP 0 + b | isExact b = b
    a + LitP 0 | isExact a = a
    a@(LitP _) + b@NonLitP | isExact a = b+a  -- Move literals to the right
    OpP BiAdd a (LitP' b) + LitP c | isExact' a = CExp a + value (b+c)
    OpP BiSub a (LitP' b) + LitP c | isExact' a = CExp a + value (c-b)
    a + LitP b | b < 0, isExact a = a - value (negate b)
    a + b = constFold $ sugarSym (T $ Op BiAdd) a b

    LitP 0 - b | isExact b = negate b
    a - LitP 0 | isExact a = a
    a@(LitP _) - b@NonLitP | isExact a = negate b - negate a  -- Move literals to the right
    OpP BiAdd a (LitP' b) - LitP c | isExact' a = CExp a + value (b-c)
    OpP BiSub a (LitP' b) - LitP c | isExact' a = CExp a - value (b+c)
    a - LitP b | b < 0, isExact a = a + value (negate b)
    a - b = constFold $ sugarSym (T $ Op BiSub) a b

    LitP 0 * b | isExact b = value 0
    a * LitP 0 | isExact a = value 0
    LitP 1 * b | isExact b = b
    a * LitP 1 | isExact a = a
    a@(LitP _) * b@NonLitP | isExact a = b*a  -- Move literals to the right
    OpP BiMul a (LitP' b) * LitP c | isExact' a = CExp a * value (b*c)
    a * b = constFold $ sugarSym (T $ Op BiMul) a b

    negate (UOpP UnNeg a)  | isExact' a = CExp a
    negate (OpP BiAdd a b) | isExact' a = negate (CExp a) - CExp b
    negate (OpP BiSub a b) | isExact' a = CExp b - CExp a
    negate (OpP BiMul a b) | isExact' a = CExp a * negate (CExp b)
      -- Negate the right operand, because literals are moved to the right
      -- in multiplications
    negate a = constFold $ sugarSym (T $ UOp UnNeg) a

    abs    = error "abs not implemented for CExp"
    signum = error "signum not implemented for CExp"

instance (Fractional a, Ord a, CType a) => Fractional (CExp a)
  where
    fromRational = value . fromRational
    a / b = constFold $ sugarSym (T $ Op BiDiv) a b

    recip = error "recip not implemented for CExp"

instance (Floating a, Ord a, CType a) => Floating (CExp a)
  where
    pi = constant (addGlobal pi_def) "EDSL_PI" pi
      where
        pi_def = [cedecl|$esc:("#define EDSL_PI 3.141592653589793")|]
          -- This is the value of `pi :: Double`.
          -- Apparently there is no standard C99 definition of pi.
    a ** b = constFold $ sugarSym (T $ Fun (addInclude "<math.h>") "pow" (**)) a b
    sin a  = constFold $ sugarSym (T $ Fun (addInclude "<math.h>") "sin" sin) a
    cos a  = constFold $ sugarSym (T $ Fun (addInclude "<math.h>") "cos" cos) a

-- | Integer division truncated toward zero
quot_ :: (Integral a, CType a) => CExp a -> CExp a -> CExp a
quot_ (LitP 0) b = 0
quot_ a (LitP 1) = a
quot_ a b
    | a == b     = 1
quot_ a b        = constFold $ sugarSym (T $ Op BiQuot) a b

-- | Integer remainder satisfying
--
-- > (x `quot_` y)*y + (x #% y) == x
(#%) :: (Integral a, CType a) => CExp a -> CExp a -> CExp a
LitP 0 #% _          = 0
_      #% LitP 1     = 0
a      #% b | a == b = 0
a      #% b          = constFold $ sugarSym (T $ Op BiRem) a b

round_ :: (RealFrac a, Integral b, CType b) => CExp a -> CExp b
round_ = constFold . sugarSym (T $ Fun (addInclude "<math.h>") "lround" round)

-- | Integral type casting
i2n :: (Integral a, Num b, CType b) => CExp a -> CExp b
i2n a = constFold $ sugarSym (T $ Cast (fromInteger . toInteger)) a

-- | Cast integer to 'Bool'
i2b :: Integral a => CExp a -> CExp Bool
i2b a = constFold $ sugarSym (T $ Cast (/=0)) a

-- | Boolean negation
not_ :: CExp Bool -> CExp Bool
not_ (UOpP UnNot a)  = CExp a
not_ (OpP BiEq a b)  = CExp a #!= CExp b
not_ (OpP BiNEq a b) = CExp a #== CExp b
not_ (OpP BiLt a b)  = CExp a #>= CExp b
not_ (OpP BiGt a b)  = CExp a #<= CExp b
not_ (OpP BiLe a b)  = CExp a #> CExp b
not_ (OpP BiGe a b)  = CExp a #< CExp b
not_ a = constFold $ sugarSym (T $ UOp UnNot) a

-- | Logical and
(#&&) :: CExp Bool -> CExp Bool -> CExp Bool
LitP True  #&& b          = b
LitP False #&& b          = false
a          #&& LitP True  = a
a          #&& LitP False = false
a          #&& b          = constFold $ sugarSym (T $ Op BiAnd) a b

-- | Logical or
(#||) :: CExp Bool -> CExp Bool -> CExp Bool
LitP True  #|| b          = true
LitP False #|| b          = b
a          #|| LitP True  = true
a          #|| LitP False = a
a          #|| b          = constFold $ sugarSym (T $ Op BiOr) a b

-- | Equality
(#==) :: (Eq a, CType a) => CExp a -> CExp a -> CExp Bool
a #== b
    | a == b, isExact a = true
    | otherwise         = constFold $ sugarSym (T $ Op BiEq) a b

-- | In-equality
(#!=) :: (Eq a, CType a) => CExp a -> CExp a -> CExp Bool
a #!= b
    | a == b, isExact a = false
    | otherwise         = constFold $ sugarSym (T $ Op BiNEq) a b

(#<) :: (Ord a, CType a) => CExp a -> CExp a -> CExp Bool
a #< b
    | a == b, isExact a = false
    | otherwise         = constFold $ sugarSym (T $ Op BiLt) a b

(#>) :: (Ord a, CType a) => CExp a -> CExp a -> CExp Bool
a #> b
    | a == b, isExact a = false
    | otherwise         = constFold $ sugarSym (T $ Op BiGt) a b

(#<=) :: (Ord a, CType a) => CExp a -> CExp a -> CExp Bool
a #<= b
    | a == b, isExact a = true
    | otherwise         = constFold $ sugarSym (T $ Op BiLe) a b

(#>=) :: (Ord a, CType a) => CExp a -> CExp a -> CExp Bool
a #>= b
    | a == b, isExact a = true
    | otherwise         = constFold $ sugarSym (T $ Op BiGe) a b

infix 4 #==, #!=, #<, #>, #<=, #>=

-- | Conditional expression
cond :: CType a
    => CExp Bool  -- ^ Condition
    -> CExp a     -- ^ True branch
    -> CExp a     -- ^ False branch
    -> CExp a
cond (LitP c) t f = if c then t else f
cond c t f
    | t == f = t
cond (UOpP UnNot a) t f = cond (CExp a) f t
cond c t f = constFold $ sugarSym (T Cond) c t f

-- | Condition operator; use as follows:
--
-- > cond1 ? a $
-- > cond2 ? b $
-- > cond3 ? c $
-- >         default
(?) :: CType a
    => CExp Bool  -- ^ Condition
    -> CExp a     -- ^ True branch
    -> CExp a     -- ^ False branch
    -> CExp a
(?) = cond

infixl 1 ?

-- | Array indexing
(#!) :: (CType a, Integral i, Ix i) => IArr i a -> CExp i -> CExp a
arr #! i = sugarSym (T $ ArrIx arr) i



--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

#if MIN_VERSION_syntactic(3,0,0)
deriveSymbol ''Sym
#endif

#if MIN_VERSION_syntactic(3,0,0)
instance Render Sym
  where
    renderSym (Lit a _)      = a
    renderSym (Const _ a _)  = a
    renderSym (Fun _ name _) = name
    renderSym (UOp op)       = show $ unaryOp op
    renderSym (Op op)        = show $ binaryOp op
    renderSym (Cast _)       = "cast"
    renderSym (Var v)        = v
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

#else

instance Semantic Sym
  where
    semantics (Lit s a)      = Sem s a
    semantics (Const _ s a)  = Sem s a
    semantics (Fun _ name f) = Sem name f
    semantics (UOp uop)      = Sem (show $ unaryOp uop) (evalUnary uop)
    semantics (Op bop)       = Sem (show $ binaryOp bop) (evalBinary bop)
    semantics (Cast f)       = Sem "cast" f
    semantics (Var v)        = Sem v $ error $ "evaluating free variable: " ++ v

instance Equality Sym
  where
    equal    = equalDefault
    exprHash = exprHashDefault

instance Semantic T
  where
    semantics (T s) = semantics s

instance Equality T
  where
    equal (T s) (T t) = equal s t
    exprHash (T s)    = exprHash s

#endif

deriving instance Eq (CExp a)
  -- Must be placed here due to the sequential dependencies introduced by
  -- Template Haskell

