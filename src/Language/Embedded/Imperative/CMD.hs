{-# LANGUAGE CPP #-}

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
    -- * Pointers
  , IsPointer (..)
  , PtrCMD (..)
    -- * File handling
  , Handle (..)
  , stdin
  , stdout
  , Formattable (..)
  , FileCMD (..)
  , PrintfArg (..)
    -- * C-specific commands
  , Ptr (..)
  , Object (..)
  , FunArg (..)
  , VarPredCast
  , Arg (..)
  , Assignable
  , C_CMD (..)
  ) where



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
import Data.Foldable hiding (sequence_)
import Data.Traversable
#endif

#if __GLASGOW_HASKELL__ < 708
import Data.Proxy
#endif

import Control.Monad.Operational.Higher

import Control.Monads
import Language.Embedded.Expression
import Language.Embedded.Traversal
import qualified Language.C.Syntax as C
import Language.C.Quote.C (ToIdent (..))
import Language.C.Monad
import Language.Embedded.Backend.C.Expression



--------------------------------------------------------------------------------
-- * References
--------------------------------------------------------------------------------

-- | Mutable reference
data Ref a
    = RefComp VarId
    | RefEval (IORef a)
  deriving Typeable

instance ToIdent (Ref a)
  where
    toIdent (RefComp r) = C.Id r

-- | Commands for mutable references
data RefCMD exp (prog :: * -> *) a
  where
    NewRef  :: VarPred exp a => String -> RefCMD exp prog (Ref a)
    InitRef :: VarPred exp a => String -> exp a -> RefCMD exp prog (Ref a)
    GetRef  :: VarPred exp a => Ref a -> RefCMD exp prog (exp a)
    SetRef  :: VarPred exp a => Ref a -> exp a -> RefCMD exp prog ()
      -- `VarPred` for `SetRef` is not needed for code generation, but it can be useful when
      -- interpreting with a dynamically typed store. `VarPred` can then be used to supply a
      -- `Typeable` dictionary for casting.
    UnsafeFreezeRef :: VarPred exp a => Ref a -> RefCMD exp prog (exp a)
      -- Like `GetRef` but without using a fresh variable for the result. This
      -- is only safe if the reference is never written to after the freezing.
#if  __GLASGOW_HASKELL__>=708
  deriving Typeable
#endif

instance HFunctor (RefCMD exp)
  where
    hfmap _ (NewRef base)       = NewRef base
    hfmap _ (InitRef base a)    = InitRef base a
    hfmap _ (GetRef r)          = GetRef r
    hfmap _ (SetRef r a)        = SetRef r a
    hfmap _ (UnsafeFreezeRef r) = UnsafeFreezeRef r

instance FreeExp exp => DryInterp (RefCMD exp)
  where
    dryInterp (NewRef base)    = liftM RefComp $ freshStr base
    dryInterp (InitRef base _) = liftM RefComp $ freshStr base
    dryInterp (GetRef _)       = liftM varExp $ freshStr "v"
    dryInterp (SetRef _ _)     = return ()
    dryInterp (UnsafeFreezeRef (RefComp v)) = return $ varExp v

type instance IExp (RefCMD e)       = e
type instance IExp (RefCMD e :+: i) = e



--------------------------------------------------------------------------------
-- * Arrays
--------------------------------------------------------------------------------

-- | Mutable array
data Arr i a
    = ArrComp VarId
    | ArrEval (IORef (IOArray i a))
        -- The `IORef` is needed in order to make the `IsPointer` instance
  deriving Typeable

-- | Immutable array
data IArr i a
    = IArrComp VarId
    | IArrEval (Array i a)
        -- The `IORef` is needed in order to make the `IsPointer` instance
  deriving Typeable

-- In a way, it's not terribly useful to have `Arr` parameterized on the index
-- type, since it's required to be an integer type, and it doesn't really matter
-- which integer type is used since we can always cast between them.
--
-- Another option would be to remove the parameter and allow any integer type
-- when indexing (and use e.g. `IOArray Word32` for evaluation). However this
-- has the big downside of losing type inference. E.g. the statement
-- `getArr arr 0` would be ambiguously typed.
--
-- Yet another option is to hard-code a specific index type. But this would
-- limit the use of arrays to specific platforms.
--
-- So in the end, the above representation seems like a good trade-off. A client
-- of `imperative-edsl` may always chose to make a wrapper interface that uses
-- a specific index type.

instance ToIdent (Arr i a)
  where
    toIdent (ArrComp arr) = C.Id arr

instance ToIdent (IArr i a)
  where
    toIdent (IArrComp arr) = C.Id arr

-- | Commands for mutable arrays
data ArrCMD exp (prog :: * -> *) a
  where
    NewArr  :: (VarPred exp a, VarPred exp i, Integral i, Ix i) => String -> exp i -> ArrCMD exp prog (Arr i a)
    InitArr :: (VarPred exp a, VarPred exp i, Integral i, Ix i) => String -> [a] -> ArrCMD exp prog (Arr i a)
    GetArr  :: (VarPred exp a, VarPred exp i, Integral i, Ix i) => exp i -> Arr i a -> ArrCMD exp prog (exp a)
    SetArr  :: (VarPred exp a, VarPred exp i, Integral i, Ix i) => exp i -> exp a -> Arr i a -> ArrCMD exp prog ()
    CopyArr :: (VarPred exp a, VarPred exp i, Integral i, Ix i) => Arr i a -> Arr i a -> exp i -> ArrCMD exp prog ()
    UnsafeFreezeArr :: (VarPred exp a, VarPred exp i, Integral i, Ix i) => Arr i a -> ArrCMD exp prog (IArr i a)
    UnsafeThawArr   :: (VarPred exp a, VarPred exp i, Integral i, Ix i) => IArr i a -> ArrCMD exp prog (Arr i a)
#if  __GLASGOW_HASKELL__>=708
  deriving Typeable
#endif
  -- Not all `VarPred` constraints are needed by the back ends in
  -- imperative-edsl, but they may still be useful for other back ends.

instance HFunctor (ArrCMD exp)
  where
    hfmap _ (NewArr base n)       = NewArr base n
    hfmap _ (InitArr base as)     = InitArr base as
    hfmap _ (GetArr i arr)        = GetArr i arr
    hfmap _ (SetArr i a arr)      = SetArr i a arr
    hfmap _ (CopyArr a1 a2 l)     = CopyArr a1 a2 l
    hfmap _ (UnsafeFreezeArr arr) = UnsafeFreezeArr arr
    hfmap _ (UnsafeThawArr arr)   = UnsafeThawArr arr

instance FreeExp exp => DryInterp (ArrCMD exp)
  where
    dryInterp (NewArr base _)  = liftM ArrComp $ freshStr base
    dryInterp (InitArr base _) = liftM ArrComp $ freshStr base
    dryInterp (GetArr _ _)     = liftM varExp $ freshStr "v"
    dryInterp (SetArr _ _ _)   = return ()
    dryInterp (CopyArr _ _ _)  = return ()
    dryInterp (UnsafeFreezeArr (ArrComp arr)) = return (IArrComp arr)
    dryInterp (UnsafeThawArr (IArrComp arr))  = return (ArrComp arr)

type instance IExp (ArrCMD e)       = e
type instance IExp (ArrCMD e :+: i) = e



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

data ControlCMD exp prog a
  where
    If     :: exp Bool -> prog () -> prog () -> ControlCMD exp prog ()
    While  :: prog (exp Bool) -> prog () -> ControlCMD exp prog ()
    For    :: (VarPred exp n, Integral n) =>
              IxRange (exp n) -> (exp n -> prog ()) -> ControlCMD exp prog ()
    Break  :: ControlCMD exp prog ()
    Assert :: exp Bool -> String -> ControlCMD exp prog ()

instance HFunctor (ControlCMD exp)
  where
    hfmap g (If c t f)        = If c (g t) (g f)
    hfmap g (While cont body) = While (g cont) (g body)
    hfmap g (For ir body)     = For ir (g . body)
    hfmap _ Break             = Break
    hfmap _ (Assert cond msg) = Assert cond msg

instance DryInterp (ControlCMD exp)
  where
    dryInterp (If _ _ _)   = return ()
    dryInterp (While _ _)  = return ()
    dryInterp (For _ _)    = return ()
    dryInterp Break        = return ()
    dryInterp (Assert _ _) = return ()

type instance IExp (ControlCMD e)       = e
type instance IExp (ControlCMD e :+: i) = e



--------------------------------------------------------------------------------
-- * Pointers
--------------------------------------------------------------------------------

-- The reason for not implementing `SwapPtr` using the `Ptr` type is that it's
-- (currently) not possible to interpret `Ptr` in `IO`.

-- | Types that are represented as a pointers in C
class ToIdent a => IsPointer a
  where
    runSwapPtr :: a -> a -> IO ()

instance IsPointer (Arr i a)
  where
    runSwapPtr (ArrEval arr1) (ArrEval arr2) = do
        arr1' <- readIORef arr1
        arr2' <- readIORef arr2
        writeIORef arr1 arr2'
        writeIORef arr2 arr1'

data PtrCMD (prog :: * -> *) a
  where
    SwapPtr :: IsPointer a => a -> a -> PtrCMD prog ()

instance HFunctor PtrCMD
  where
    hfmap _ (SwapPtr a b) = SwapPtr a b

instance DryInterp PtrCMD
  where
    dryInterp (SwapPtr _ _) = return ()



--------------------------------------------------------------------------------
-- * File handling
--------------------------------------------------------------------------------

-- | File handle
data Handle
    = HandleComp VarId
    | HandleEval IO.Handle
  deriving Typeable

instance ToIdent Handle
  where
    toIdent (HandleComp h) = C.Id h

-- | Handle to stdin
stdin :: Handle
stdin = HandleComp "stdin"

-- | Handle to stdout
stdout :: Handle
stdout = HandleComp "stdout"

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

data FileCMD exp (prog :: * -> *) a
  where
    FOpen   :: FilePath -> IOMode                       -> FileCMD exp prog Handle
    FClose  :: Handle                                   -> FileCMD exp prog ()
    FEof    :: VarPred exp Bool => Handle               -> FileCMD exp prog (exp Bool)
    FPrintf :: Handle -> String -> [PrintfArg exp]      -> FileCMD exp prog ()
    FGet    :: (Formattable a, VarPred exp a) => Handle -> FileCMD exp prog (exp a)

data PrintfArg exp where
  PrintfArg :: (Printf.PrintfArg a, VarPred exp a) => exp a -> PrintfArg exp

instance HFunctor (FileCMD exp)
  where
    hfmap _ (FOpen file mode)     = FOpen file mode
    hfmap _ (FClose hdl)          = FClose hdl
    hfmap _ (FPrintf hdl form as) = FPrintf hdl form as
    hfmap _ (FGet hdl)            = FGet hdl
    hfmap _ (FEof hdl)            = FEof hdl

instance FreeExp exp => DryInterp (FileCMD exp)
  where
    dryInterp (FOpen _ _)     = liftM HandleComp $ freshStr "h"
    dryInterp (FClose _)      = return ()
    dryInterp (FPrintf _ _ _) = return ()
    dryInterp (FGet _)        = liftM varExp $ freshStr "v"
    dryInterp (FEof _)        = liftM varExp $ freshStr "v"

type instance IExp (FileCMD e)       = e
type instance IExp (FileCMD e :+: i) = e



--------------------------------------------------------------------------------
-- * C-specific commands
--------------------------------------------------------------------------------

-- | Pointer
newtype Ptr a = PtrComp {ptrId :: VarId}
  deriving Typeable

instance ToIdent (Ptr a)
  where
    toIdent = C.Id . ptrId

-- | Abstract object
data Object = Object
    { pointed    :: Bool
    , objectType :: String
    , objectId   :: VarId
    }
  deriving (Eq, Show, Ord, Typeable)

instance ToIdent Object
  where
    toIdent (Object _ _ o) = C.Id o

data FunArg exp where
  FunArg :: Arg arg => arg exp -> FunArg exp

-- | Evidence that @`VarPred` exp1@ implies @`VarPred` exp2@
type VarPredCast exp1 exp2 = forall a b .
    VarPred exp1 a => Proxy a -> (VarPred exp2 a => b) -> b

class Arg arg where
  mkArg   :: CompExp exp => arg exp -> CGen C.Exp
  mkParam :: CompExp exp => arg exp -> CGen C.Param

  -- | Map over the expression(s) in an argument
  mapArg  :: VarPredCast exp1 exp2
          -> (forall a . VarPred exp1 a => exp1 a -> exp2 a)
          -> arg exp1
          -> arg exp2

  -- | Monadic map over the expression(s) in an argument
  mapMArg :: Monad m
          => VarPredCast exp1 exp2
          -> (forall a . VarPred exp1 a => exp1 a -> m (exp2 a))
          -> arg exp1
          -> m (arg exp2)

instance Arg FunArg where
  mkArg   (FunArg arg) = mkArg arg
  mkParam (FunArg arg) = mkParam arg
  mapArg  predCast f (FunArg arg) = FunArg (mapArg predCast f arg)
  mapMArg predCast f (FunArg arg) = liftM FunArg (mapMArg predCast f arg)

class ToIdent obj => Assignable obj

instance Assignable (Ref a)
instance Assignable (Arr i a)
instance Assignable (IArr i a)
instance Assignable (Ptr a)
instance Assignable Object

data C_CMD exp (prog :: * -> *) a
  where
    NewPtr   :: VarPred exp a => String -> C_CMD exp prog (Ptr a)
    PtrToArr :: Ptr a -> C_CMD exp prog (Arr i a)
    NewObject
        :: String  -- Base name
        -> String  -- Type
        -> Bool    -- Pointed?
        -> C_CMD exp prog Object
    AddInclude    :: String       -> C_CMD exp prog ()
    AddDefinition :: C.Definition -> C_CMD exp prog ()
    AddExternFun  :: VarPred exp res
                  => String
                  -> proxy (exp res)
                  -> [FunArg exp]
                  -> C_CMD exp prog ()
    AddExternProc :: String -> [FunArg exp] -> C_CMD exp prog ()
    CallFun       :: VarPred exp a => String -> [FunArg exp] -> C_CMD exp prog (exp a)
    CallProc      :: Assignable obj => Maybe obj -> String -> [FunArg exp] -> C_CMD exp prog ()

instance HFunctor (C_CMD exp)
  where
    hfmap _ (NewPtr base)               = NewPtr base
    hfmap _ (PtrToArr p)                = PtrToArr p
    hfmap _ (NewObject base p t)        = NewObject base p t
    hfmap _ (AddInclude incl)           = AddInclude incl
    hfmap _ (AddDefinition def)         = AddDefinition def
    hfmap _ (AddExternFun fun res args) = AddExternFun fun res args
    hfmap _ (AddExternProc proc args)   = AddExternProc proc args
    hfmap _ (CallFun fun args)          = CallFun fun args
    hfmap _ (CallProc obj proc args)    = CallProc obj proc args

instance FreeExp exp => DryInterp (C_CMD exp)
  where
    dryInterp (NewPtr base)          = liftM PtrComp $ freshStr base
    dryInterp (PtrToArr (PtrComp p)) = return $ ArrComp p
    dryInterp (NewObject base t p)   = liftM (Object p t) $ freshStr base
    dryInterp (AddInclude _)         = return ()
    dryInterp (AddDefinition _)      = return ()
    dryInterp (AddExternFun _ _ _)   = return ()
    dryInterp (AddExternProc _ _)    = return ()
    dryInterp (CallFun _ _)          = liftM varExp $ freshStr "v"
    dryInterp (CallProc _ _ _)       = return ()

type instance IExp (C_CMD e)       = e
type instance IExp (C_CMD e :+: i) = e



--------------------------------------------------------------------------------
-- * Running commands
--------------------------------------------------------------------------------

runRefCMD :: forall exp prog a . EvalExp exp => RefCMD exp prog a -> IO a
runRefCMD (InitRef _ a)                     = fmap RefEval $ newIORef $ evalExp a
runRefCMD (NewRef _)                        = fmap RefEval $ newIORef $ error "reading uninitialized reference"
runRefCMD (SetRef (RefEval r) a)            = writeIORef r $ evalExp a
runRefCMD (GetRef (RefEval (r :: IORef b))) = fmap valExp $ readIORef r
runRefCMD (UnsafeFreezeRef r)               = runRefCMD (GetRef r)

runArrCMD :: EvalExp exp => ArrCMD exp prog a -> IO a
runArrCMD (NewArr _ n)   = fmap ArrEval . newIORef =<< newArray_ (0, fromIntegral (evalExp n)-1)
runArrCMD (InitArr _ as) = fmap ArrEval . newIORef =<< newListArray (0, genericLength as - 1) as
runArrCMD (GetArr i (ArrEval arr)) = do
    arr'  <- readIORef arr
    let i' = evalExp i
    (l,h) <- getBounds arr'
    if i'<l || i'>h
      then error $ "getArr: index "
                ++ show (toInteger i')
                ++ " out of bounds "
                ++ show (toInteger l, toInteger h)
      else fmap valExp $ readArray arr' i'
runArrCMD (SetArr i a (ArrEval arr)) = do
    arr'  <- readIORef arr
    let i' = evalExp i
    (l,h) <- getBounds arr'
    if i'<l || i'>h
      then error $ "setArr: index "
                ++ show (toInteger i')
                ++ " out of bounds "
                ++ show (toInteger l, toInteger h)
      else writeArray arr' (fromIntegral i') (evalExp a)
runArrCMD (CopyArr (ArrEval arr1) (ArrEval arr2) l) = do
    arr1'  <- readIORef arr1
    arr2'  <- readIORef arr2
    let l' = evalExp l
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
runArrCMD (UnsafeFreezeArr (ArrEval arr)) =
    fmap IArrEval . freeze =<< readIORef arr
runArrCMD (UnsafeThawArr (IArrEval arr)) =
    fmap ArrEval . newIORef =<< thaw arr

runControlCMD :: EvalExp exp => ControlCMD exp IO a -> IO a
runControlCMD (If c t f)        = if evalExp c then t else f
runControlCMD (While cont body) = loop
  where loop = do
          c <- cont
          when (evalExp c) $ body >> loop
runControlCMD (For (lo,step,hi) body) = loop (evalExp lo)
  where
    incl = borderIncl hi
    hi'  = evalExp $ borderVal hi
    cont i
      | incl && (step>=0) = i <= hi'
      | incl && (step<0)  = i >= hi'
      | step >= 0         = i <  hi'
      | step < 0          = i >  hi'
    loop i
      | cont i    = body (valExp i) >> loop (i + fromIntegral step)
      | otherwise = return ()
runControlCMD Break = error "cannot run programs involving break"
runControlCMD (Assert cond msg) = unless (evalExp cond) $ error $
    "Assertion failed: " ++ msg

runPtrCMD :: PtrCMD prog a -> IO a
runPtrCMD (SwapPtr a b) = runSwapPtr a b

evalHandle :: Handle -> IO.Handle
evalHandle (HandleEval h)        = h
evalHandle (HandleComp "stdin")  = IO.stdin
evalHandle (HandleComp "stdout") = IO.stdout

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

evalFPrintf :: EvalExp exp =>
    [PrintfArg exp] -> (forall r . Printf.HPrintfType r => r) -> IO ()
evalFPrintf []            pf = pf
evalFPrintf (PrintfArg a:as) pf = evalFPrintf as (pf $ evalExp a)

runFileCMD :: EvalExp exp => FileCMD exp IO a -> IO a
runFileCMD (FOpen file mode)              = fmap HandleEval $ IO.openFile file mode
runFileCMD (FClose (HandleEval h))        = IO.hClose h
runFileCMD (FClose (HandleComp "stdin"))  = return ()
runFileCMD (FClose (HandleComp "stdout")) = return ()
runFileCMD (FPrintf h format as)          = evalFPrintf as (Printf.hPrintf (evalHandle h) format)
runFileCMD (FGet h)   = do
    w <- readWord $ evalHandle h
    case reads w of
        [(f,"")] -> return $ valExp f
        _        -> error $ "fget: no parse (input " ++ show w ++ ")"
runFileCMD (FEof h) = fmap valExp $ IO.hIsEOF $ evalHandle h

runC_CMD :: C_CMD exp IO a -> IO a
runC_CMD (NewPtr base)        = error $ "cannot run programs involving newPtr (base name " ++ base ++ ")"
runC_CMD (PtrToArr p)         = error "cannot run programs involving ptrToArr"
runC_CMD (NewObject base _ _) = error $ "cannot run programs involving newObject (base name " ++ base ++ ")"
runC_CMD (AddInclude _)       = return ()
runC_CMD (AddDefinition _)    = return ()
runC_CMD (AddExternFun _ _ _) = return ()
runC_CMD (AddExternProc _ _)  = return ()
runC_CMD (CallFun _ _)        = error "cannot run programs involving callFun"
runC_CMD (CallProc _ _ _)     = error "cannot run programs involving callProc"

instance EvalExp exp => Interp (RefCMD exp)     IO where interp = runRefCMD
instance EvalExp exp => Interp (ArrCMD exp)     IO where interp = runArrCMD
instance EvalExp exp => Interp (ControlCMD exp) IO where interp = runControlCMD
instance                Interp PtrCMD           IO where interp = runPtrCMD
instance EvalExp exp => Interp (FileCMD exp)    IO where interp = runFileCMD
instance EvalExp exp => Interp (C_CMD exp)      IO where interp = runC_CMD

