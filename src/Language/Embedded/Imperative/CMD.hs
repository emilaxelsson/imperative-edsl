{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Imperative commands. These commands can be used with the 'Program' monad,
-- and different command types can be combined using (':+:').
--
-- These commands are general imperative constructs independent of the back end,
-- except for 'C_CMD' which is C-specific.

module Language.Embedded.Imperative.CMD
  ( -- * References
    Ref (..)
  , RefCMD (..)
    -- * Arrays
  , Arr (..)
  , IArr (..)
  , ArrCMD (..)
    -- * Control flow
  , Border (..)
  , borderVal
  , borderIncl
  , IxRange
  , ControlCMD (..)
    -- * Generic pointer manipulation
  , IsPointer (..)
  , PtrCMD (..)
    -- * File handling
  , Handle (..)
  , stdin
  , stdout
  , PrintfArg (..)
  , mapPrintfArg
  , mapPrintfArgM
  , Formattable (..)
  , FileCMD (..)
    -- * C-specific commands
  , Ptr (..)
  , Object (..)
  , Arg (..)
  , FunArg (..)
  , mapFunArg
  , mapFunArgM
  , Assignable
  , C_CMD (..)
  ) where



import Control.Monad.Reader
import Data.Array
import Data.Array.IO
import Data.Char (isSpace)
import Data.Int
import Data.IORef
import Data.List
import Data.Typeable
import Data.Word
import System.IO (IOMode (..))
import qualified System.IO as IO
import qualified Text.Printf as Printf

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
import Data.Foldable hiding (sequence_)
import Data.Traversable (Traversable, traverse)
#endif

import Control.Monad.Operational.Higher

import Control.Monads
import Language.Embedded.Expression
import Language.Embedded.Traversal

-- C-specific imports:
import qualified Language.C.Syntax as C
import Language.C.Quote.C
import Language.C.Monad (CGen)
import Language.Embedded.Backend.C.Expression



--------------------------------------------------------------------------------
-- * References
--------------------------------------------------------------------------------

-- | Mutable reference
data Ref a
    = RefComp VarId
    | RefRun (IORef a)
  deriving Typeable

instance ToIdent (Ref a) where toIdent (RefComp r) = C.Id r

-- | Commands for mutable references
data RefCMD fs a
  where
    NewRef  :: pred a => String -> RefCMD (Param3 prog exp pred) (Ref a)
    InitRef :: pred a => String -> exp a -> RefCMD (Param3 prog exp pred) (Ref a)
    GetRef  :: pred a => Ref a -> RefCMD (Param3 prog exp pred) (Val a)
    SetRef  :: pred a => Ref a -> exp a -> RefCMD (Param3 prog exp pred) ()
      -- `pred a` for `SetRef` is not needed for code generation, but it can be
      -- useful when interpreting with a dynamically typed store. It can then be
      -- used e.g. to supply a `Typeable` constraint for casting.
    UnsafeFreezeRef :: pred a => Ref a -> RefCMD (Param3 prog exp pred) (Val a)
      -- Like `GetRef` but without using a fresh variable for the result. This
      -- is only safe if the reference is never written to after the freezing.
#if  __GLASGOW_HASKELL__>=708
  deriving Typeable
#endif

instance HFunctor RefCMD
  where
    hfmap _ (NewRef base)       = NewRef base
    hfmap _ (InitRef base a)    = InitRef base a
    hfmap _ (GetRef r)          = GetRef r
    hfmap _ (SetRef r a)        = SetRef r a
    hfmap _ (UnsafeFreezeRef r) = UnsafeFreezeRef r

instance HBifunctor RefCMD
  where
    hbimap _ _ (NewRef base)       = NewRef base
    hbimap _ f (InitRef base a)    = InitRef base (f a)
    hbimap _ _ (GetRef r)          = GetRef r
    hbimap _ f (SetRef r a)        = SetRef r (f a)
    hbimap _ _ (UnsafeFreezeRef r) = UnsafeFreezeRef r

instance (RefCMD :<: instr) => Reexpressible RefCMD instr
  where
    reexpressInstrEnv reexp (NewRef base)       = lift $ singleInj $ NewRef base
    reexpressInstrEnv reexp (InitRef base a)    = lift . singleInj . InitRef base =<< reexp a
    reexpressInstrEnv reexp (GetRef r)          = lift $ singleInj $ GetRef r
    reexpressInstrEnv reexp (SetRef r a)        = lift . singleInj . SetRef r =<< reexp a
    reexpressInstrEnv reexp (UnsafeFreezeRef r) = lift $ singleInj $ UnsafeFreezeRef r

instance DryInterp RefCMD
  where
    dryInterp (NewRef base)    = liftM RefComp $ freshStr base
    dryInterp (InitRef base _) = liftM RefComp $ freshStr base
    dryInterp (GetRef _)       = liftM ValComp $ freshStr "v"
    dryInterp (SetRef _ _)     = return ()
    dryInterp (UnsafeFreezeRef (RefComp v)) = return $ ValComp v



--------------------------------------------------------------------------------
-- * Arrays
--------------------------------------------------------------------------------

-- | Mutable array
data Arr i a
    = ArrComp VarId
    | ArrRun (IORef (IOArray i a))
        -- The `IORef` is needed in order to make the `IsPointer` instance
  deriving Typeable

-- | Immutable array
data IArr i a
    = IArrComp VarId
    | IArrRun (Array i a)
  deriving (Eq, Show, Typeable)

-- In a way, it's not terribly useful to have `Arr` parameterized on the index
-- type, since it's required to be an integer type, and it doesn't really matter
-- which integer type is used since we can always cast between them.
--
-- Another option would be to remove the parameter and allow any integer type
-- when indexing (and use e.g. `IOArray Word32` for running). However this has
-- the big downside of losing type inference. E.g. the statement `getArr arr 0`
-- would be ambiguously typed.
--
-- Yet another option is to hard-code a specific index type. But this would
-- limit the use of arrays to specific platforms.
--
-- So in the end, the above representation seems like a good trade-off. A client
-- of `imperative-edsl` may always chose to make a wrapper interface that uses
-- a specific index type.

instance ToIdent (Arr i a)  where toIdent (ArrComp arr)  = C.Id arr
instance ToIdent (IArr i a) where toIdent (IArrComp arr) = C.Id arr

-- | Commands for mutable arrays
data ArrCMD fs a
  where
    NewArr  :: (pred a, Integral i, Ix i) => String -> exp i -> ArrCMD (Param3 prog exp pred) (Arr i a)
    InitArr :: (pred a, Integral i, Ix i) => String -> [a] -> ArrCMD (Param3 prog exp pred) (Arr i a)
    GetArr  :: (pred a, Integral i, Ix i) => exp i -> Arr i a -> ArrCMD (Param3 prog exp pred) (Val a)
    SetArr  :: (pred a, Integral i, Ix i) => exp i -> exp a -> Arr i a -> ArrCMD (Param3 prog exp pred) ()
    CopyArr :: (pred a, Integral i, Ix i) => Arr i a -> Arr i a -> exp i -> ArrCMD (Param3 prog exp pred) ()
    UnsafeFreezeArr :: (pred a, Integral i, Ix i) => Arr i a -> ArrCMD (Param3 prog exp pred) (IArr i a)
    UnsafeThawArr   :: (pred a, Integral i, Ix i) => IArr i a -> ArrCMD (Param3 prog exp pred) (Arr i a)
#if  __GLASGOW_HASKELL__>=708
  deriving Typeable
#endif
  -- Not all `pred` constraints are needed by the back ends in imperative-edsl,
  -- but they may still be useful for other back ends.

instance HFunctor ArrCMD
  where
    hfmap _ (NewArr base n)       = NewArr base n
    hfmap _ (InitArr base as)     = InitArr base as
    hfmap _ (GetArr i arr)        = GetArr i arr
    hfmap _ (SetArr i a arr)      = SetArr i a arr
    hfmap _ (CopyArr a1 a2 l)     = CopyArr a1 a2 l
    hfmap _ (UnsafeFreezeArr arr) = UnsafeFreezeArr arr
    hfmap _ (UnsafeThawArr arr)   = UnsafeThawArr arr

instance HBifunctor ArrCMD
  where
    hbimap _ f (NewArr base n)       = NewArr base (f n)
    hbimap _ _ (InitArr base as)     = InitArr base as
    hbimap _ f (GetArr i arr)        = GetArr (f i) arr
    hbimap _ f (SetArr i a arr)      = SetArr (f i) (f a) arr
    hbimap _ f (CopyArr a1 a2 l)     = CopyArr a1 a2 (f l)
    hbimap _ _ (UnsafeFreezeArr arr) = UnsafeFreezeArr arr
    hbimap _ _ (UnsafeThawArr arr)   = UnsafeThawArr arr

instance (ArrCMD :<: instr) => Reexpressible ArrCMD instr
  where
    reexpressInstrEnv reexp (NewArr base n)       = lift . singleInj . NewArr base =<< reexp n
    reexpressInstrEnv reexp (InitArr base as)     = lift $ singleInj $ InitArr base as
    reexpressInstrEnv reexp (GetArr i arr)        = lift . singleInj . flip GetArr arr =<< reexp i
    reexpressInstrEnv reexp (SetArr i a arr)      = do i' <- reexp i; a' <- reexp a; lift $ singleInj $ SetArr i' a' arr
    reexpressInstrEnv reexp (CopyArr a1 a2 l)     = lift . singleInj . CopyArr a1 a2 =<< reexp l
    reexpressInstrEnv reexp (UnsafeFreezeArr arr) = lift $ singleInj $ UnsafeFreezeArr arr
    reexpressInstrEnv reexp (UnsafeThawArr arr)   = lift $ singleInj $ UnsafeThawArr arr

instance DryInterp ArrCMD
  where
    dryInterp (NewArr base _)  = liftM ArrComp $ freshStr base
    dryInterp (InitArr base _) = liftM ArrComp $ freshStr base
    dryInterp (GetArr _ _)     = liftM ValComp $ freshStr "v"
    dryInterp (SetArr _ _ _)   = return ()
    dryInterp (CopyArr _ _ _)  = return ()
    dryInterp (UnsafeFreezeArr (ArrComp arr)) = return (IArrComp arr)
    dryInterp (UnsafeThawArr (IArrComp arr))  = return (ArrComp arr)



--------------------------------------------------------------------------------
-- * Control flow
--------------------------------------------------------------------------------

data Border i = Incl i | Excl i
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | 'fromInteger' gives an inclusive border. No other methods defined.
instance Num i => Num (Border i)
  where
    fromInteger = Incl . fromInteger
    (+) = error "(+) not defined for Border"
    (-) = error "(-) not defined for Border"
    (*) = error "(*) not defined for Border"
    abs    = error "abs not defined for Border"
    signum = error "signum not defined for Border"

borderVal :: Border i -> i
borderVal (Incl i) = i
borderVal (Excl i) = i

borderIncl :: Border i -> Bool
borderIncl (Incl _) = True
borderIncl _        = False

-- | Index range
--
-- @(lo,step,hi)@
--
-- @lo@ gives the start index; @step@ gives the step length; @hi@ gives the stop
-- index which may be inclusive or exclusive.
type IxRange i = (i, Int, Border i)

data ControlCMD fs a
  where
    If     :: exp Bool -> prog () -> prog () -> ControlCMD (Param3 prog exp pred) ()
    While  :: prog (exp Bool) -> prog () -> ControlCMD (Param3 prog exp pred) ()
    For    :: (pred i, Integral i) => IxRange (exp i) -> (Val i -> prog ()) -> ControlCMD (Param3 prog exp pred) ()
    Break  :: ControlCMD (Param3 prog exp pred) ()
    Assert :: exp Bool -> String -> ControlCMD (Param3 prog exp pred) ()

instance HFunctor ControlCMD
  where
    hfmap f (If c thn els)    = If c (f thn) (f els)
    hfmap f (While cont body) = While (f cont) (f body)
    hfmap f (For rng body)    = For rng (f . body)
    hfmap _ Break             = Break
    hfmap _ (Assert cond msg) = Assert cond msg

instance HBifunctor ControlCMD
  where
    hbimap f g (If c thn els)          = If (g c) (f thn) (f els)
    hbimap f g (While cont body)       = While (f $ fmap g cont) (f body)
    hbimap f g (For (lo,step,hi) body) = For (g lo, step, fmap g hi) (f . body)
    hbimap _ _ Break                   = Break
    hbimap _ g (Assert cond msg)       = Assert (g cond) msg

instance (ControlCMD :<: instr) => Reexpressible ControlCMD instr
  where
    reexpressInstrEnv reexp (If c thn els) = do
        c' <- reexp c
        ReaderT $ \env ->
          singleInj $ If c' (runReaderT thn env) (runReaderT els env)
    reexpressInstrEnv reexp (While cont body) = ReaderT $ \env ->
        singleInj $ While
            (runReaderT (cont >>= reexp) env)
            (runReaderT body env)
    reexpressInstrEnv reexp (For (lo,step,hi) body) = do
        lo' <- reexp lo
        hi' <- traverse reexp hi
        ReaderT $ \env -> singleInj $
            For (lo',step,hi') (flip runReaderT env . body)
    reexpressInstrEnv reexp Break             = lift $ singleInj Break
    reexpressInstrEnv reexp (Assert cond msg) = lift . singleInj . flip Assert msg =<< reexp cond

instance DryInterp ControlCMD
  where
    dryInterp (If _ _ _)   = return ()
    dryInterp (While _ _)  = return ()
    dryInterp (For _ _)    = return ()
    dryInterp Break        = return ()
    dryInterp (Assert _ _) = return ()



--------------------------------------------------------------------------------
-- * Generic pointer manipulation
--------------------------------------------------------------------------------

-- The reason for not implementing `SwapPtr` using the `Ptr` type is that it's
-- (currently) not possible to interpret `Ptr` in `IO`.

-- | Types that are represented as a pointers in C
class ToIdent a => IsPointer a
  where
    runSwapPtr :: a -> a -> IO ()

instance IsPointer (Arr i a)
  where
    runSwapPtr (ArrRun arr1) (ArrRun arr2) = do
        arr1' <- readIORef arr1
        arr2' <- readIORef arr2
        writeIORef arr1 arr2'
        writeIORef arr2 arr1'

data PtrCMD fs a
  where
    SwapPtr :: IsPointer a => a -> a -> PtrCMD (Param3 prog exp pred) ()

instance HFunctor   PtrCMD where hfmap _    (SwapPtr a b) = SwapPtr a b
instance HBifunctor PtrCMD where hbimap _ _ (SwapPtr a b) = SwapPtr a b

instance (PtrCMD :<: instr) => Reexpressible PtrCMD instr
  where
    reexpressInstrEnv reexp (SwapPtr a b) = lift $ singleInj (SwapPtr a b)

instance DryInterp PtrCMD
  where
    dryInterp (SwapPtr _ _) = return ()



--------------------------------------------------------------------------------
-- * File handling
--------------------------------------------------------------------------------

-- | File handle
data Handle
    = HandleComp VarId
    | HandleRun IO.Handle
  deriving Typeable

instance ToIdent Handle where toIdent (HandleComp h) = C.Id h

-- | Handle to stdin
stdin :: Handle
stdin = HandleComp "stdin"

-- | Handle to stdout
stdout :: Handle
stdout = HandleComp "stdout"

data PrintfArg exp
  where
    PrintfArg :: Printf.PrintfArg a => exp a -> PrintfArg exp

mapPrintfArg
    :: (forall a . exp1 a -> exp2 a)
    -> PrintfArg exp1 -> PrintfArg exp2
mapPrintfArg f (PrintfArg exp) = PrintfArg (f exp)

mapPrintfArgM :: Monad m
    => (forall a . exp1 a -> m (exp2 a))
    -> PrintfArg exp1 -> m (PrintfArg exp2)
mapPrintfArgM f (PrintfArg exp) = liftM PrintfArg (f exp)

-- | Values that can be printed\/scanned using @printf@\/@scanf@
class (Typeable a, Read a, Printf.PrintfArg a) => Formattable a
  where
    formatSpecifier :: Proxy a -> String

instance Formattable Int    where formatSpecifier _ = "%d"
instance Formattable Int8   where formatSpecifier _ = "%d"
instance Formattable Int16  where formatSpecifier _ = "%d"
instance Formattable Int32  where formatSpecifier _ = "%d"
instance Formattable Int64  where formatSpecifier _ = "%d"
instance Formattable Word   where formatSpecifier _ = "%u"
instance Formattable Word8  where formatSpecifier _ = "%u"
instance Formattable Word16 where formatSpecifier _ = "%u"
instance Formattable Word32 where formatSpecifier _ = "%u"
instance Formattable Word64 where formatSpecifier _ = "%u"
instance Formattable Float  where formatSpecifier _ = "%f"
instance Formattable Double where formatSpecifier _ = "%f"

data FileCMD fs a
  where
    FOpen   :: FilePath -> IOMode                  -> FileCMD (Param3 prog exp pred) Handle
    FClose  :: Handle                              -> FileCMD (Param3 prog exp pred) ()
    FEof    :: Handle                              -> FileCMD (Param3 prog exp pred) (Val Bool)
    FPrintf :: Handle -> String -> [PrintfArg exp] -> FileCMD (Param3 prog exp pred) ()
    FGet    :: (pred a, Formattable a) => Handle   -> FileCMD (Param3 prog exp pred) (Val a)

instance HFunctor FileCMD
  where
    hfmap _ (FOpen file mode)     = FOpen file mode
    hfmap _ (FClose hdl)          = FClose hdl
    hfmap _ (FPrintf hdl form as) = FPrintf hdl form as
    hfmap _ (FGet hdl)            = FGet hdl
    hfmap _ (FEof hdl)            = FEof hdl

instance HBifunctor FileCMD
  where
    hbimap _ _ (FOpen file mode)     = FOpen file mode
    hbimap _ _ (FClose hdl)          = FClose hdl
    hbimap _ f (FPrintf hdl form as) = FPrintf hdl form (map (mapPrintfArg f) as)
    hbimap _ _ (FGet hdl)            = FGet hdl
    hbimap _ _ (FEof hdl)            = FEof hdl

instance (FileCMD :<: instr) => Reexpressible FileCMD instr
  where
    reexpressInstrEnv reexp (FOpen file mode)   = lift $ singleInj $ FOpen file mode
    reexpressInstrEnv reexp (FClose h)          = lift $ singleInj $ FClose h
    reexpressInstrEnv reexp (FEof h)            = lift $ singleInj $ FEof h
    reexpressInstrEnv reexp (FPrintf h form as) = lift . singleInj . FPrintf h form =<< mapM (mapPrintfArgM reexp) as
    reexpressInstrEnv reexp (FGet h)            = lift $ singleInj $ FGet h

instance DryInterp FileCMD
  where
    dryInterp (FOpen _ _)     = liftM HandleComp $ freshStr "h"
    dryInterp (FClose _)      = return ()
    dryInterp (FPrintf _ _ _) = return ()
    dryInterp (FGet _)        = liftM ValComp $ freshStr "v"
    dryInterp (FEof _)        = liftM ValComp $ freshStr "v"



--------------------------------------------------------------------------------
-- * C-specific commands
--------------------------------------------------------------------------------

-- | Pointer
newtype Ptr (a :: *) = PtrComp {ptrId :: VarId}
  deriving (Eq, Show, Typeable)

instance ToIdent (Ptr a) where toIdent = C.Id . ptrId

-- | Abstract object
data Object = Object
    { pointed    :: Bool
    , objectType :: String
    , objectId   :: VarId
    }
  deriving (Eq, Show, Ord, Typeable)

instance ToIdent Object where toIdent (Object _ _ o) = C.Id o

class Arg arg pred
  where
    mkArg   :: arg pred -> CGen C.Exp
    mkParam :: arg pred -> CGen C.Param

data FunArg exp pred
  where
    ValArg   :: pred a => exp a -> FunArg exp pred
    AddrArg  :: FunArg exp pred -> FunArg exp pred
    DerefArg :: FunArg exp pred -> FunArg exp pred
    FunArg   :: Arg arg pred => arg pred -> FunArg exp pred

instance (CompExp exp, CompTypeClass ct) => Arg (FunArg exp) ct
  where
    mkArg (ValArg a) = compExp a
    mkArg (AddrArg arg) = do
        e <- mkArg arg
        return [cexp| &$e |]
    mkArg (DerefArg arg) = do
        e <- mkArg arg
        return [cexp| *$e |]
    mkArg (FunArg a) = mkArg a

    mkParam (ValArg (a :: exp a)) = do
        t <- compType (Proxy :: Proxy ct) (Proxy :: Proxy a)
        return [cparam| $ty:t |]
    mkParam (AddrArg arg) = do
      p <- mkParam arg
      case p of
         C.Param mid spec decl loc -> return $ C.Param mid spec (C.Ptr [] decl loc) loc
         _ -> error "mkParam for Addr: cannot deal with antiquotes"
    mkParam (DerefArg arg) = do
      p <- mkParam arg
      case p of
         C.Param mid spec (C.Ptr [] decl _) loc -> return $ C.Param mid spec decl loc
         C.Param _ _ _ _ -> error "mkParam for Deref: cannot dereference non-pointer parameter"
         _ -> error "mkParam for Deref: cannot deal with antiquotes"
    mkParam (FunArg a) = mkParam a

mapFunArg ::
    (forall a . exp1 a -> exp2 a) -> FunArg exp1 pred -> FunArg exp2 pred
mapFunArg f (ValArg a)   = ValArg (f a)
mapFunArg f (AddrArg a)  = AddrArg $ mapFunArg f a
mapFunArg f (DerefArg a) = DerefArg $ mapFunArg f a
mapFunArg f (FunArg a)   = FunArg a

mapFunArgM :: Monad m
    => (forall a . exp1 a -> m (exp2 a))
    -> FunArg exp1 pred
    -> m (FunArg exp2 pred)
mapFunArgM f (ValArg a)   = liftM ValArg (f a)
mapFunArgM f (AddrArg a)  = liftM AddrArg $ mapFunArgM f a
mapFunArgM f (DerefArg a) = liftM DerefArg $ mapFunArgM f a
mapFunArgM f (FunArg a)   = return (FunArg a)

class ToIdent obj => Assignable obj

instance Assignable (Ref a)
instance Assignable (Arr i a)
instance Assignable (IArr i a)
instance Assignable (Ptr a)
instance Assignable Object

data C_CMD fs a
  where
    NewPtr   :: pred a => String -> C_CMD (Param3 prog exp pred) (Ptr a)
    PtrToArr :: Ptr a -> C_CMD (Param3 prog exp pred) (Arr i a)
    NewObject
        :: String  -- Base name
        -> String  -- Type
        -> Bool    -- Pointed?
        -> C_CMD (Param3 prog exp pred) Object
    AddInclude    :: String       -> C_CMD (Param3 prog exp pred) ()
    AddDefinition :: C.Definition -> C_CMD (Param3 prog exp pred) ()
    AddExternFun  :: pred res => String -> proxy res -> [FunArg exp pred] -> C_CMD (Param3 prog exp pred) ()
    AddExternProc :: String -> [FunArg exp pred] -> C_CMD (Param3 prog exp pred) ()
    CallFun       :: pred a => String -> [FunArg exp pred] -> C_CMD (Param3 prog exp pred) (Val a)
    CallProc      :: Assignable obj => Maybe obj -> String -> [FunArg exp pred] -> C_CMD (Param3 prog exp pred) ()
    InModule      :: String -> prog () -> C_CMD (Param3 prog exp pred) ()

instance HFunctor C_CMD
  where
    hfmap _ (NewPtr base)             = NewPtr base
    hfmap _ (PtrToArr p)              = PtrToArr p
    hfmap _ (NewObject base p t)      = NewObject base p t
    hfmap _ (AddInclude incl)         = AddInclude incl
    hfmap _ (AddDefinition def)       = AddDefinition def
    hfmap _ (AddExternFun fun p args) = AddExternFun fun p args
    hfmap _ (AddExternProc proc args) = AddExternProc proc args
    hfmap _ (CallFun fun args)        = CallFun fun args
    hfmap _ (CallProc obj proc args)  = CallProc obj proc args
    hfmap f (InModule mod prog)       = InModule mod (f prog)

instance HBifunctor C_CMD
  where
    hbimap _ _ (NewPtr base)             = NewPtr base
    hbimap _ _ (PtrToArr p)              = PtrToArr p
    hbimap _ _ (NewObject base p t)      = NewObject base p t
    hbimap _ _ (AddInclude incl)         = AddInclude incl
    hbimap _ _ (AddDefinition def)       = AddDefinition def
    hbimap _ f (AddExternFun fun p args) = AddExternFun fun p (map (mapFunArg f) args)
    hbimap _ f (AddExternProc proc args) = AddExternProc proc (map (mapFunArg f) args)
    hbimap _ f (CallFun fun args)        = CallFun fun (map (mapFunArg f) args)
    hbimap _ f (CallProc obj proc args)  = CallProc obj proc (map (mapFunArg f) args)
    hbimap f _ (InModule mod prog)       = InModule mod (f prog)

instance (C_CMD :<: instr) => Reexpressible C_CMD instr
  where
    reexpressInstrEnv reexp (NewPtr base)             = lift $ singleInj $ NewPtr base
    reexpressInstrEnv reexp (PtrToArr p)              = lift $ singleInj $ PtrToArr p
    reexpressInstrEnv reexp (NewObject base p t)      = lift $ singleInj $ NewObject base p t
    reexpressInstrEnv reexp (AddInclude incl)         = lift $ singleInj $ AddInclude incl
    reexpressInstrEnv reexp (AddDefinition def)       = lift $ singleInj $ AddDefinition def
    reexpressInstrEnv reexp (AddExternFun fun p args) = lift . singleInj . AddExternFun fun p =<< mapM (mapFunArgM reexp) args
    reexpressInstrEnv reexp (AddExternProc proc args) = lift . singleInj . AddExternProc proc =<< mapM (mapFunArgM reexp) args
    reexpressInstrEnv reexp (CallFun fun args)        = lift . singleInj . CallFun fun =<< mapM (mapFunArgM reexp) args
    reexpressInstrEnv reexp (CallProc obj proc args)  = lift . singleInj . CallProc obj proc =<< mapM (mapFunArgM reexp) args
    reexpressInstrEnv reexp (InModule mod prog)       = ReaderT $ \env -> singleInj $ InModule mod (runReaderT prog env)

instance DryInterp C_CMD
  where
    dryInterp (NewPtr base)          = liftM PtrComp $ freshStr base
    dryInterp (PtrToArr (PtrComp p)) = return $ ArrComp p
    dryInterp (NewObject base t p)   = liftM (Object p t) $ freshStr base
    dryInterp (AddInclude _)         = return ()
    dryInterp (AddDefinition _)      = return ()
    dryInterp (AddExternFun _ _ _)   = return ()
    dryInterp (AddExternProc _ _)    = return ()
    dryInterp (CallFun _ _)          = liftM ValComp $ freshStr "v"
    dryInterp (CallProc _ _ _)       = return ()
    dryInterp (InModule _ _)         = return ()



--------------------------------------------------------------------------------
-- * Running commands
--------------------------------------------------------------------------------

runRefCMD :: RefCMD (Param3 IO IO pred) a -> IO a
runRefCMD (NewRef _)              = fmap RefRun $ newIORef $ error "reading uninitialized reference"
runRefCMD (InitRef _ a)           = fmap RefRun . newIORef =<< a
runRefCMD (SetRef (RefRun r) a)   = writeIORef r =<< a
runRefCMD (GetRef (RefRun r))     = ValRun <$> readIORef r
runRefCMD cmd@(UnsafeFreezeRef r) = runRefCMD (GetRef r `asTypeOf` cmd)

runArrCMD :: ArrCMD (Param3 IO IO pred) a -> IO a
runArrCMD (NewArr _ n) = do
    n'  <- n
    arr <- newArray_ (0, fromIntegral n'-1)
    ArrRun <$> newIORef arr
runArrCMD (InitArr _ as) =
    fmap ArrRun . newIORef =<< newListArray (0, genericLength as - 1) as
runArrCMD (GetArr i (ArrRun arr)) = do
    arr'  <- readIORef arr
    i'    <- i
    (l,h) <- getBounds arr'
    if i'<l || i'>h
      then error $ "getArr: index "
                ++ show (toInteger i')
                ++ " out of bounds "
                ++ show (toInteger l, toInteger h)
      else ValRun <$> readArray arr' i'
runArrCMD (SetArr i a (ArrRun arr)) = do
    arr'  <- readIORef arr
    i'    <- i
    a'    <- a
    (l,h) <- getBounds arr'
    if i'<l || i'>h
      then error $ "setArr: index "
                ++ show (toInteger i')
                ++ " out of bounds "
                ++ show (toInteger l, toInteger h)
      else writeArray arr' (fromIntegral i') a'
runArrCMD (CopyArr (ArrRun arr1) (ArrRun arr2) l) = do
    arr1'  <- readIORef arr1
    arr2'  <- readIORef arr2
    l'     <- l
    (0,h1) <- getBounds arr1'
    (0,h2) <- getBounds arr2'
    if l'>h2+1
    then error $ "copyArr: cannot copy "
              ++ show (toInteger l')
              ++ " elements from array with "
              ++ show (toInteger (h2+1))
              ++ " allocated elements"
    else if l'>h1+1
    then error $ "copyArr: cannot copy "
              ++ show (toInteger l')
              ++ " elements to array with "
              ++ show (toInteger (h1+1))
              ++ " allocated elements"
    else sequence_
      [ readArray arr2' i >>= writeArray arr1' i | i <- genericTake l' [0..] ]
runArrCMD (UnsafeFreezeArr (ArrRun arr)) =
    fmap IArrRun . freeze =<< readIORef arr
runArrCMD (UnsafeThawArr (IArrRun arr)) =
    fmap ArrRun . newIORef =<< thaw arr

runControlCMD :: ControlCMD (Param3 IO IO pred) a -> IO a
runControlCMD (If c t f)        = c >>= \c' -> if c' then t else f
runControlCMD (While cont body) = loop
  where loop = do
          c <- join cont
          when c (body >> loop)
runControlCMD (For (lo,step,hi) body) = do
    lo' <- lo
    hi' <- borderVal hi
    loop lo' hi'
  where
    incl = borderIncl hi
    cont i h
      | incl && (step>=0) = i <= h
      | incl && (step<0)  = i >= h
      | step >= 0         = i <  h
      | step < 0          = i >  h
    loop i h
      | cont i h  = body (ValRun i) >> loop (i + fromIntegral step) h
      | otherwise = return ()
runControlCMD Break = error "cannot run programs involving break"
runControlCMD (Assert cond msg) = do
    cond' <- cond
    unless cond' $ error $ "Assertion failed: " ++ msg

runPtrCMD :: PtrCMD (Param3 IO IO pred) a -> IO a
runPtrCMD (SwapPtr a b) = runSwapPtr a b

runHandle :: Handle -> IO.Handle
runHandle (HandleRun h)         = h
runHandle (HandleComp "stdin")  = IO.stdin
runHandle (HandleComp "stdout") = IO.stdout

readWord :: IO.Handle -> IO String
readWord h = do
    eof <- IO.hIsEOF h
    if eof
    then return ""
    else do
      c  <- IO.hGetChar h
      if isSpace c
      then return ""
      else do
        cs <- readWord h
        return (c:cs)

runFPrintf :: [PrintfArg IO] -> (forall r . Printf.HPrintfType r => r) -> IO ()
runFPrintf []               pf = pf
runFPrintf (PrintfArg a:as) pf = a >>= \a' -> runFPrintf as (pf a')

runFileCMD :: FileCMD (Param3 IO IO pred) a -> IO a
runFileCMD (FOpen file mode)              = HandleRun <$> IO.openFile file mode
runFileCMD (FClose (HandleRun h))         = IO.hClose h
runFileCMD (FClose (HandleComp "stdin"))  = return ()
runFileCMD (FClose (HandleComp "stdout")) = return ()
runFileCMD (FPrintf h format as)          = runFPrintf as (Printf.hPrintf (runHandle h) format)
runFileCMD (FGet h)   = do
    w <- readWord $ runHandle h
    case reads w of
        [(f,"")] -> return $ ValRun f
        _        -> error $ "fget: no parse (input " ++ show w ++ ")"
runFileCMD (FEof h) = fmap ValRun $ IO.hIsEOF $ runHandle h

runC_CMD :: C_CMD (Param3 IO IO pred) a -> IO a
runC_CMD (NewPtr base)        = error $ "cannot run programs involving newPtr (base name " ++ base ++ ")"
runC_CMD (PtrToArr p)         = error "cannot run programs involving ptrToArr"
runC_CMD (NewObject base _ _) = error $ "cannot run programs involving newObject (base name " ++ base ++ ")"
runC_CMD (AddInclude _)       = return ()
runC_CMD (AddDefinition _)    = return ()
runC_CMD (AddExternFun _ _ _) = return ()
runC_CMD (AddExternProc _ _)  = return ()
runC_CMD (CallFun _ _)        = error "cannot run programs involving callFun"
runC_CMD (CallProc _ _ _)     = error "cannot run programs involving callProc"
runC_CMD (InModule _ prog)    = prog

instance InterpBi RefCMD     IO (Param1 pred) where interpBi = runRefCMD
instance InterpBi ArrCMD     IO (Param1 pred) where interpBi = runArrCMD
instance InterpBi ControlCMD IO (Param1 pred) where interpBi = runControlCMD
instance InterpBi PtrCMD     IO (Param1 pred) where interpBi = runPtrCMD
instance InterpBi FileCMD    IO (Param1 pred) where interpBi = runFileCMD
instance InterpBi C_CMD      IO (Param1 pred) where interpBi = runC_CMD

