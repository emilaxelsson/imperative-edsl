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
import Data.Maybe
#if __GLASGOW_HASKELL__ < 710
import Data.Monoid
#endif
import Data.Typeable

import Language.Syntactic
import Language.Syntactic.Functional (Denotation)
import Language.Syntactic.TH

import Language.C.Quote.C
import Language.C.Syntax (Type, UnOp (..), BinOp (..), Exp (UnOp, BinOp))
import qualified Language.C.Syntax as C

import Language.C.Monad
import Language.Embedded.Expression
import Language.Embedded.Backend.C
import Language.Embedded.Imperative.CMD (IArr (..))



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
  -- Only needed because GHC 7.8 can't represent tuple constraints (like
  -- `MonadC`) in Template Haskell.

-- | Syntactic symbols for C
data Sym sig
  where
    -- Literal
    Lit   :: String -> a -> Sym (Full a)
    -- Predefined constant
    Const :: String -> a -> Sym (Full a)
      -- The difference between `Lit` and `Const` is that the latter gets turned
      -- into a variable in the C code. It is like `Var`, except that it can
      -- also be evaluated.
    -- Function call
    Fun   :: Signature sig => String -> Denotation sig -> Sym sig
    -- Unary operator
    UOp   :: Unary (a -> b) -> Sym (a :-> Full b)
    -- Binary operator
    Op    :: Binary (a -> b -> c) -> Sym (a :-> b :-> Full c)
    -- Type casting (ignored when generating code)
    Cast  :: (a -> b) -> Sym (a :-> Full b)
    -- Conditional
    Cond  :: Sym (Bool :-> a :-> a :-> Full a)
    -- Variable (only for compilation)
    Var   :: VarId -> Sym (Full a)
    -- Unsafe array indexing
    ArrIx :: (Integral i, Ix i) => IArr i a -> Sym (i :-> Full a)
    -- Attach extra code to an expression
    WithCode :: SupportCode -> Sym (a :-> Full a)

deriveSymbol ''Sym

instance Render Sym
  where
    renderSym (Lit a _)    = a
    renderSym (Const a _)  = a
    renderSym (Fun name _) = name
    renderSym (UOp op)     = show $ unaryOp op
    renderSym (Op op)      = show $ binaryOp op
    renderSym (Cast _)     = "cast"
    renderSym (Var v)      = v
    renderSym (ArrIx (IArrComp arr)) = "ArrIx " ++ arr
    renderSym (ArrIx _)              = "ArrIx ..."
    renderSym (WithCode _) = "WithCode ..."

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

data T sig
  where
    T :: CType (DenResult sig) => { unT :: Sym sig } -> T sig

-- | C expression
newtype CExp a = CExp {unCExp :: ASTF T a}
  deriving (Eq)

instance Syntactic (CExp a)
  where
    type Domain (CExp a)   = T
    type Internal (CExp a) = a
    desugar = unCExp
    sugar   = CExp

evalSym :: Sym sig -> Denotation sig
evalSym (Lit _ a)   = a
evalSym (Const _ a) = a
evalSym (Fun _ f)   = f
evalSym (UOp uop)   = evalUnary uop
evalSym (Op bop)    = evalBinary bop
evalSym (Cast f)    = f
evalSym Cond        = \c t f -> if c then t else f
evalSym (ArrIx (IArrRun arr)) = \i ->
    if i<l || i>h
      then error $ "index "
                ++ show (toInteger i)
                ++ " out of bounds "
                ++ show (toInteger l, toInteger h)
      else arr!i
  where
    (l,h) = bounds arr
evalSym (WithCode _) = id
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
    type FreePred CExp = CType
    constExp a = CExp $ Sym $ T $ Lit (show a) a
    varExp = CExp . Sym . T . Var

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
    go (Var v) Nil   = touchVar v >> return [cexp| $id:v |]
    go (Lit _ a) Nil = cLit a
    go (Const const _) Nil = do
      touchVar const
      return [cexp| $id:const |]
    go (Fun fun _) args = do
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
      return [cexp|($ty:t) $a'|]
          -- Explicit casting is usually not needed. But sometimes it is. For
          -- example
          --
          --     printf("%f",i);
          --
          -- gives an error if `i` is an integer. The most robust option is
          -- probably to always have explicit casts. In many cases it probably
          -- also makes the generated code more readable.
    go Cond (c :* t :* f :* Nil) = do
      c' <- compCExp' c
      t' <- compCExp' t
      f' <- compCExp' f
      return $ C.Cond c' t' f' mempty
    go (ArrIx arr) (i :* Nil) = do
      i' <- compCExp' i
      touchVar arr
      return [cexp| $id:arr[$i'] |]
    go (WithCode code) (a :* Nil) = code >> compCExp' a

instance CompExp CExp where compExp = compCExp

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
-- * User interface
--------------------------------------------------------------------------------

-- | Construct a literal expression
value :: CType a => a -> CExp a
value a = CExp $ Sym $ T $ Lit (show a) a

-- | Predefined constant
constant :: CType a
    => String  -- ^ Name of constant
    -> a       -- ^ Value of constant
    -> CExp a
constant const val = CExp $ Sym $ T $ Const const val

-- | Create a named variable
variable :: CType a => VarId -> CExp a
variable = CExp . Sym . T . Var

withCode :: CType a => (forall m . MonadC m => m ()) -> CExp a -> CExp a
withCode code = CExp . smartSym' (T $ WithCode code) . unCExp

true, false :: CExp Bool
true  = withCode (addInclude "<stdbool.h>") $ constant "true" True
false = withCode (addInclude "<stdbool.h>") $ constant "false" False

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

-- | Integer division truncated toward zero
quot_ :: (Eq a, Integral a, CType a) => CExp a -> CExp a -> CExp a
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

-- | Integral type casting
i2n :: (Integral a, Num b, CType b) => CExp a -> CExp b
i2n a = constFold $ sugarSym (T $ Cast (fromInteger . toInteger)) a

-- | Cast integer to 'Bool'
i2b :: Integral a => CExp a -> CExp Bool
i2b a = constFold $ sugarSym (T $ Cast (/=0)) a

-- | Cast 'Bool' to integer
b2i :: (Integral a, CType a) => CExp Bool -> CExp a
b2i a = constFold $ sugarSym (T $ Cast (\c -> if c then 1 else 0)) a

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

