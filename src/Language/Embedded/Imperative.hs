{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Deep embedding of imperative programs. The embedding is parameterized on the expression
-- language.

module Language.Embedded.Imperative where



import Data.Array.IO
import Data.IORef
import Data.Typeable

import Control.Monad.Operational.Compositional
import Data.Constraint
import Language.C.Quote.C
import qualified Language.C.Syntax as C

import Language.C.Monad



----------------------------------------------------------------------------------------------------
-- * Interpretation of expressions
----------------------------------------------------------------------------------------------------

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
class CompExp exp
  where
    -- | Variable expressions
    varExp  :: VarPred exp a => VarId -> exp a

    -- | Compilation of expressions
    compExp :: exp a -> CGen C.Exp

-- | Variable identifier
type VarId = String

-- | Universal predicate
class    Any a
instance Any a

-- | Predicate conjunction
class    (p1 a, p2 a) => (p1 :/\: p2) a
instance (p1 a, p2 a) => (p1 :/\: p2) a



----------------------------------------------------------------------------------------------------
-- * Commands
----------------------------------------------------------------------------------------------------

data Ref a
    = RefComp String
    | RefEval (IORef a)

-- | Commands for mutable references
data RefCMD p exp (prog :: * -> *) a
  where
    NewRef          :: p a => RefCMD p exp prog (Ref a)
    InitRef         :: p a => exp a -> RefCMD p exp prog (Ref a)
    GetRef          :: p a => Ref a -> RefCMD p exp prog (exp a)
    SetRef          ::        Ref a -> exp a -> RefCMD p exp prog ()
    UnsafeFreezeRef :: p a => Ref a -> RefCMD p exp prog (exp a)

instance MapInstr (RefCMD p exp)
  where
    imap f NewRef              = NewRef
    imap f (InitRef a)         = InitRef a
    imap f (GetRef r)          = GetRef r
    imap f (SetRef r a)        = SetRef r a
    imap f (UnsafeFreezeRef r) = UnsafeFreezeRef r

data Arr a
    = ArrComp String
    | ArrEval (IOArray Int a)

-- | Commands for mutable arrays
data ArrCMD p exp (prog :: * -> *) a
  where
    NewArr :: (p a, Integral n) => exp n -> exp a -> ArrCMD p exp prog (Arr (exp a))
    GetArr :: (p a, Integral n) => exp n -> Arr (exp a) -> ArrCMD p exp prog (exp a)
    SetArr :: Integral n        => exp n -> exp a -> Arr (exp a) -> ArrCMD p exp prog ()

instance MapInstr (ArrCMD p exp)
  where
    imap f (NewArr n a)        = NewArr n a
    imap f (GetArr i arr)      = GetArr i arr
    imap f (SetArr i a arr)    = SetArr i a arr



----------------------------------------------------------------------------------------------------
-- * Running commands
----------------------------------------------------------------------------------------------------

runRefCMD :: EvalExp exp => RefCMD (VarPred exp) exp prog a -> IO a
runRefCMD (InitRef a)                   = fmap RefEval $ newIORef $ evalExp a
runRefCMD NewRef                        = fmap RefEval $ newIORef (error "Reading uninitialized reference")
runRefCMD (GetRef (RefEval r))          = fmap litExp  $ readIORef r
runRefCMD (SetRef (RefEval r) a)        = writeIORef r $ evalExp a
runRefCMD (UnsafeFreezeRef (RefEval r)) = fmap litExp  $ readIORef r

runArrCMD :: EvalExp exp => ArrCMD (VarPred exp) exp prog a -> IO a
runArrCMD (NewArr n a)               = fmap ArrEval $ newArray (0, fromIntegral (evalExp n) - 1) a
runArrCMD (GetArr i (ArrEval arr))   = readArray arr (fromIntegral (evalExp i))
runArrCMD (SetArr i a (ArrEval arr)) = writeArray arr (fromIntegral (evalExp i)) a

instance (EvalExp exp, pred ~ VarPred exp) => Interp (RefCMD pred exp) IO where interp = runRefCMD
instance (EvalExp exp, pred ~ VarPred exp) => Interp (ArrCMD pred exp) IO where interp = runArrCMD



----------------------------------------------------------------------------------------------------
-- * Compiling commands
----------------------------------------------------------------------------------------------------

compTypeRep :: TypeRep -> C.Type
compTypeRep trep = case show trep of
    "Bool"  -> [cty| int   |]
    "Int"   -> [cty| int   |]  -- todo: should only use fix-width Haskell ints
    "Float" -> [cty| float |]

typeOfP1 :: forall proxy a . Typeable a => proxy a -> TypeRep
typeOfP1 _ = typeOf (undefined :: a)

typeOfP2 :: forall proxy1 proxy2 a . Typeable a => proxy1 (proxy2 a) -> TypeRep
typeOfP2 _ = typeOf (undefined :: a)

compRefCMD :: CompExp exp => RefCMD (Typeable :/\: VarPred exp) exp prog a -> CGen a
compRefCMD cmd@NewRef = do
    let t = compTypeRep (typeOfP2 cmd)
    sym <- gensym "r"
    addLocal [cdecl| $ty:t $id:sym; |]
    return $ RefComp sym
compRefCMD cmd@(InitRef exp) = do
    let t = compTypeRep (typeOfP2 cmd)
    sym <- gensym "r"
    v   <- compExp exp
    addLocal [cdecl| $ty:t $id:sym; |]
    addStm   [cstm| $id:sym = $v; |]
    return $ RefComp sym
compRefCMD cmd@(GetRef (RefComp ref)) = do
    let t = compTypeRep (typeOfP2 cmd)
    sym <- gensym "r"
    addLocal [cdecl| $ty:t $id:sym; |]
    addStm   [cstm| $id:sym = $id:ref; |]
    return $ varExp sym
compRefCMD (SetRef (RefComp ref) exp) = do
    v <- compExp exp
    addStm [cstm| $id:ref = $v; |]
compRefCMD (UnsafeFreezeRef (RefComp ref)) = return $ varExp ref

compArrCMD :: CompExp exp => ArrCMD (Typeable :/\: VarPred exp) exp prog a -> CGen a
compArrCMD (NewArr size init) = do
    addInclude "<string.h>"
    sym <- gensym "a"
    v   <- compExp size
    i   <- compExp init -- todo: use this with memset
    addLocal [cdecl| float $id:sym[ $v ]; |] -- todo: get real type
    addStm   [cstm| memset($id:sym, $i, sizeof( $id:sym )); |]
    return $ ArrComp sym
-- compArrCMD (NewArr size init) = do
--     addInclude "<string.h>"
--     sym <- gensym "a"
--     v   <- compExp size
--     i   <- compExp init -- todo: use this with memset
--     addLocal [cdecl| float* $id:sym = calloc($v, sizeof(float)); |] -- todo: get real type
--     addFinalStm [cstm| free($id:sym); |]
--     addInclude "<stdlib.h>"
--     return $ ArrComp sym
compArrCMD (GetArr expi (ArrComp arr)) = do
    sym <- gensym "a"
    i   <- compExp expi
    addLocal [cdecl| float $id:sym; |] -- todo: get real type
    addStm   [cstm| $id:sym = $id:arr[ $i ]; |]
    return $ varExp sym
compArrCMD (SetArr expi expv (ArrComp arr)) = do
    v <- compExp expv
    i <- compExp expi
    addStm [cstm| $id:arr[ $i ] = $v; |]

instance (CompExp exp, pred ~ (Typeable :/\: VarPred exp)) => Interp (RefCMD pred exp) CGen where interp = compRefCMD
instance (CompExp exp, pred ~ (Typeable :/\: VarPred exp)) => Interp (ArrCMD pred exp) CGen where interp = compArrCMD



----------------------------------------------------------------------------------------------------
-- * User interface
----------------------------------------------------------------------------------------------------

-- | Create an uninitialized reference
newRef :: pred a => ProgramT (RefCMD pred exp) m (Ref a)
newRef = singleton NewRef

-- | Create an initialized reference
initRef :: pred a => exp a -> ProgramT (RefCMD pred exp) m (Ref a)
initRef = singleton . InitRef

-- | Get the contents of reference
getRef :: pred a => Ref a -> ProgramT (RefCMD pred exp) m (exp a)
getRef r = singleton (GetRef r)

-- | Set the contents of reference
setRef :: pred a => Ref a -> exp a -> ProgramT (RefCMD pred exp) m ()
setRef r = singleton . SetRef r

-- | Modify the contents of reference
modifyRef :: (pred a, Monad m) => Ref a -> (exp a -> exp a) -> ProgramT (RefCMD pred exp) m ()
modifyRef r f = getRef r >>= setRef r . f

-- | Freeze the contents of reference (only safe if the reference is never accessed again)
unsafeFreezeRef :: pred a => Ref a -> ProgramT (RefCMD pred exp) m (exp a)
unsafeFreezeRef r = singleton (UnsafeFreezeRef r)

