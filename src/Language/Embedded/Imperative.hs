{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Deep embedding of imperative programs. The embedding is parameterized on the expression
-- language.

module Language.Embedded.Imperative
  ( module Data.TypePredicates
  , module Control.Monad.Operational.Compositional
  , module Language.Embedded.Expression

    -- * Working with instruction sets
  , IExp
  , injE
  , prjE
  , singleE

    -- * Types
  , FunArg (..)
  , Scannable (..)

    -- * Commands
  , Ref (..)
  , RefCMD (..)
  , Arr (..)
  , ArrCMD (..)
  , ControlCMD (..)
  , IOMode (..)
  , Handle (..)
  , stdin
  , stdout
  , FileCMD (..)
  , CallCMD (..)

    -- * Running commands
  , runRefCMD
  , runArrCMD
  , runControlCMD
  , runFileCMD

    -- * User interface
  , newRef
  , initRef
  , getRef
  , setRef
  , modifyRef
  , unsafeFreezeRef
  , newArr
  , getArr
  , setArr
  , iff
  , ifE
  , while
  , whileE
  , break
  , fopen
  , fclose
  , feof
  , PrintfArg
  , PrintfType
  , fprintf
  , fput
  , fget
  , printf
  , callFun
  , callProc
  , getTime
  ) where



import Prelude hiding (break)

import Control.Monad (when)
import Data.Array.IO
import Data.Char (isSpace)
import Data.IORef
import Data.Typeable
import System.IO (IOMode (..))
import qualified System.IO as IO
import System.IO.Unsafe
import Text.Printf (PrintfArg)
import qualified Text.Printf as Printf

import Data.Constraint
import Data.Proxy

import Data.ALaCarte
import Data.TypePredicates
import Control.Monad.Operational.Compositional
import Language.Embedded.Expression
import Language.C.Quote.C
import qualified Language.C.Syntax as C



----------------------------------------------------------------------------------------------------
-- * Working with instruction sets
----------------------------------------------------------------------------------------------------

-- | Extract the expression type from an instruction set. 'IExp' is needed to avoid types like
-- @(`SomeInstr` exp `:<:` i) => `Program` i ()@. Here it is not possible to constrain @exp@ by
-- constraining @i@, so the instance search will always fail. Functions like 'injE' solve this by
-- using 'IExp' to determine @exp@ from @i@. For this to work, one must use an instruction set @i@
-- that has an instance of 'IExp'. By using instruction sets of the form
-- @(`RefCMD` SomeExp `:+:` ...)@, such instances are obtained for free (see the available instances
-- defined in this module). Then functions like 'injE' will determine the predicate and expression
-- type from the first summand, which may or may not be the desired behavior. It is of course also
-- possible to make custom instruction types with custom instances of 'IExp'.
type family IExp (i :: (* -> *) -> * -> *) :: * -> *

-- | Inject an instruction that is parameterized by an expression type
injE :: (i (IExp instr) :<: instr) => i (IExp instr) m a -> instr m a
injE = inj

-- | Project an instruction that is parameterized by an expression type
prjE :: (i (IExp instr) :<: instr) => instr m a -> Maybe (i (IExp instr) m a)
prjE = prj

-- | Create a program from an instruction that is parameterized by an expression type
singleE :: (i (IExp instr) :<: instr) => i (IExp instr) (ProgramT instr m) a -> ProgramT instr m a
singleE = singleton . inj



----------------------------------------------------------------------------------------------------
-- * Types
----------------------------------------------------------------------------------------------------

-- | A function argument with constrained existentially quantified type
data FunArg pred exp
  where
    FunArg :: pred a => exp a -> FunArg pred exp



----------------------------------------------------------------------------------------------------
-- * Commands
----------------------------------------------------------------------------------------------------

data Ref a
    = RefComp VarId
    | RefEval (IORef a)
  deriving Typeable

-- | Commands for mutable references
data RefCMD exp (prog :: * -> *) a
  where
    NewRef          :: VarPred exp a => RefCMD exp prog (Ref a)
    InitRef         :: VarPred exp a => exp a -> RefCMD exp prog (Ref a)
    GetRef          :: VarPred exp a => Ref a -> RefCMD exp prog (exp a)
    SetRef          ::                  Ref a -> exp a -> RefCMD exp prog ()
#if  __GLASGOW_HASKELL__>=708
  deriving Typeable
#endif

instance MapInstr (RefCMD exp)
  where
    imap _ NewRef              = NewRef
    imap _ (InitRef a)         = InitRef a
    imap _ (GetRef r)          = GetRef r
    imap _ (SetRef r a)        = SetRef r a

type instance IExp (RefCMD e)       = e
type instance IExp (RefCMD e :+: i) = e

data Arr n a
    = ArrComp String
    | ArrEval (IOArray Int a)
  deriving Typeable

-- | Commands for mutable arrays
data ArrCMD exp (prog :: * -> *) a
  where
    NewArr :: (VarPred exp a, VarPred exp n, Integral n) => exp n -> exp a -> ArrCMD exp prog (Arr n a)
    GetArr :: (VarPred exp a, Integral n)                => exp n -> Arr n a -> ArrCMD exp prog (exp a)
    SetArr :: Integral n                                 => exp n -> exp a -> Arr n a -> ArrCMD exp prog ()
#if  __GLASGOW_HASKELL__>=708
  deriving Typeable
#endif

instance MapInstr (ArrCMD exp)
  where
    imap _ (NewArr n a)     = NewArr n a
    imap _ (GetArr i arr)   = GetArr i arr
    imap _ (SetArr i a arr) = SetArr i a arr

type instance IExp (ArrCMD e)       = e
type instance IExp (ArrCMD e :+: i) = e

data ControlCMD exp prog a
  where
    If    :: exp Bool -> prog () -> prog () -> ControlCMD exp prog ()
    While :: prog (exp Bool) -> prog () -> ControlCMD exp prog ()
    Break :: ControlCMD exp prog ()

instance MapInstr (ControlCMD exp)
  where
    imap g (If c t f)        = If c (g t) (g f)
    imap g (While cont body) = While (g cont) (g body)
    imap _ Break             = Break

type instance IExp (ControlCMD e)       = e
type instance IExp (ControlCMD e :+: i) = e

data Handle
    = HandleComp String
    | HandleEval IO.Handle
  deriving Typeable

stdin, stdout :: Handle
stdin  = HandleComp "stdin"
stdout = HandleComp "stdout"

class Typeable a => Scannable a
  where
    scanFormatSpecifier :: Proxy a -> String

instance Scannable Int   where scanFormatSpecifier _ = "%d"
instance Scannable Float where scanFormatSpecifier _ = "%f"

data FileCMD exp (prog :: * -> *) a
  where
    FOpen   :: FilePath -> IOMode                             -> FileCMD exp prog Handle
    FClose  :: Handle                                         -> FileCMD exp prog ()
    FEof    :: VarPred exp Bool => Handle                     -> FileCMD exp prog (exp Bool)
    FPrintf :: Handle -> String -> [FunArg PrintfArg exp]     -> FileCMD exp prog ()
    FGet    :: (Read a, Scannable a, VarPred exp a) => Handle -> FileCMD exp prog (exp a)

instance MapInstr (FileCMD exp)
  where
    imap _ (FOpen file mode)     = FOpen file mode
    imap _ (FClose hdl)          = FClose hdl
    imap _ (FPrintf hdl form as) = FPrintf hdl form as
    imap _ (FGet hdl)            = FGet hdl
    imap _ (FEof hdl)            = FEof hdl

type instance IExp (FileCMD e)       = e
type instance IExp (FileCMD e :+: i) = e

data CallCMD exp (prog :: * -> *) a
  where
    AddInclude    :: String       -> CallCMD exp prog ()
    AddDefinition :: C.Definition -> CallCMD exp prog ()
    CallFun       :: VarPred exp a => String -> [FunArg Any exp] -> CallCMD exp prog (exp a)
    CallProc      ::                  String -> [FunArg Any exp] -> CallCMD exp prog ()

instance MapInstr (CallCMD exp)
  where
    imap _ (AddInclude incl)    = AddInclude incl
    imap _ (AddDefinition def)  = AddDefinition def
    imap _ (CallFun fun args)   = CallFun fun args
    imap _ (CallProc proc args) = CallProc proc args

type instance IExp (CallCMD e)       = e
type instance IExp (CallCMD e :+: i) = e



----------------------------------------------------------------------------------------------------
-- * Running commands
----------------------------------------------------------------------------------------------------

runRefCMD :: forall exp prog a . EvalExp exp => RefCMD exp prog a -> IO a
runRefCMD (InitRef a)                       = fmap RefEval $ newIORef $ evalExp a
runRefCMD NewRef                            = fmap RefEval $ newIORef $ error "reading uninitialized reference"
runRefCMD (SetRef (RefEval r) a)            = writeIORef r $ evalExp a
runRefCMD (GetRef (RefEval (r :: IORef b))) = fmap litExp $ readIORef r

runArrCMD :: forall exp prog a . EvalExp exp => ArrCMD exp prog a -> IO a
runArrCMD (NewArr n a)               = fmap ArrEval $ newArray (0, fromIntegral (evalExp n) - 1) (evalExp a)
runArrCMD (SetArr i a (ArrEval arr)) = writeArray arr (fromIntegral (evalExp i)) (evalExp a)
runArrCMD (GetArr i (ArrEval (arr :: IOArray Int b)))
    = fmap litExp $ readArray arr (fromIntegral (evalExp i))

runControlCMD :: EvalExp exp => ControlCMD exp IO a -> IO a
runControlCMD (If c t f)        = if evalExp c then t else f
runControlCMD (While cont body) = loop
  where loop = do
          c <- cont
          when (evalExp c) $ body >> loop
runControlCMD Break = error "cannot run programs involving break"

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
    [FunArg PrintfArg exp] -> (forall r . Printf.HPrintfType r => r) -> IO ()
evalFPrintf []            pf = pf
evalFPrintf (FunArg a:as) pf = evalFPrintf as (pf $ evalExp a)

runFileCMD :: EvalExp exp => FileCMD exp IO a -> IO a
runFileCMD (FOpen file mode)              = fmap HandleEval $ IO.openFile file mode
runFileCMD (FClose (HandleEval h))        = IO.hClose h
runFileCMD (FClose (HandleComp "stdin"))  = return ()
runFileCMD (FClose (HandleComp "stdout")) = return ()
runFileCMD (FPrintf h format as)          = evalFPrintf as (Printf.hPrintf (evalHandle h) format)
runFileCMD (FGet h)   = do
    w <- readWord $ evalHandle h
    case reads w of
        [(f,"")] -> return $ litExp f
        _        -> error $ "fget: no parse (input " ++ show w ++ ")"
runFileCMD (FEof h) = fmap litExp $ IO.hIsEOF $ evalHandle h

runCallCMD :: EvalExp exp => CallCMD exp IO a -> IO a
runCallCMD (AddInclude _)    = return ()
runCallCMD (AddDefinition _) = return ()
runCallCMD (CallFun _ _)     = error "cannot run programs involving callFun"
runCallCMD (CallProc _ _)    = error "cannot run programs involving callProc"

instance EvalExp exp => Interp (RefCMD exp)     IO where interp = runRefCMD
instance EvalExp exp => Interp (ArrCMD exp)     IO where interp = runArrCMD
instance EvalExp exp => Interp (ControlCMD exp) IO where interp = runControlCMD
instance EvalExp exp => Interp (FileCMD exp)    IO where interp = runFileCMD
instance EvalExp exp => Interp (CallCMD exp)    IO where interp = runCallCMD



----------------------------------------------------------------------------------------------------
-- * User interface
----------------------------------------------------------------------------------------------------

-- | Create an uninitialized reference
newRef :: (VarPred (IExp instr) a, RefCMD (IExp instr) :<: instr) => ProgramT instr m (Ref a)
newRef = singleE NewRef

-- | Create an initialized reference
initRef :: (VarPred (IExp instr) a, RefCMD (IExp instr) :<: instr) =>
    IExp instr a -> ProgramT instr m (Ref a)
initRef = singleE . InitRef

-- | Get the contents of a reference
getRef :: (VarPred (IExp instr) a, RefCMD (IExp instr) :<: instr) =>
    Ref a -> ProgramT instr m (IExp instr a)
getRef = singleE . GetRef

-- | Set the contents of a reference
setRef :: (VarPred (IExp instr) a, RefCMD (IExp instr) :<: instr) =>
    Ref a -> IExp instr a -> ProgramT instr m ()
setRef r = singleE . SetRef r

-- | Modify the contents of reference
modifyRef :: (VarPred (IExp instr) a, RefCMD (IExp instr) :<: instr, Monad m) =>
    Ref a -> (IExp instr a -> IExp instr a) -> ProgramT instr m ()
modifyRef r f = getRef r >>= setRef r . f

-- | Freeze the contents of reference (only safe if the reference is never written to after the
-- first action that makes use of the resulting expression)
unsafeFreezeRef :: (VarPred exp a, EvalExp exp, CompExp exp) => Ref a -> exp a
unsafeFreezeRef (RefEval r) = litExp (unsafePerformIO $ readIORef r)
unsafeFreezeRef (RefComp v) = varExp v

-- | Create an uninitialized an array
newArr :: (pred a, pred i, Integral i, ArrCMD (IExp instr) :<: instr, pred ~ VarPred (IExp instr)) =>
    IExp instr i -> IExp instr a -> ProgramT instr m (Arr i a)
newArr n a = singleE $ NewArr n a

-- | Set the contents of an array
getArr :: (VarPred (IExp instr) a, ArrCMD (IExp instr) :<: instr, Integral i) =>
    IExp instr i -> Arr i a -> ProgramT instr m (IExp instr a)
getArr i arr = singleE (GetArr i arr)

-- | Set the contents of an array
setArr :: (VarPred (IExp instr) a, ArrCMD (IExp instr) :<: instr, Integral i) =>
    IExp instr i -> IExp instr a -> Arr i a -> ProgramT instr m ()
setArr i a arr = singleE (SetArr i a arr)

iff :: (ControlCMD (IExp instr) :<: instr)
    => IExp instr Bool
    -> ProgramT instr m ()
    -> ProgramT instr m ()
    -> ProgramT instr m ()
iff b t f = singleE $ If b t f

ifE
    :: ( VarPred (IExp instr) a
       , ControlCMD (IExp instr) :<: instr
       , RefCMD (IExp instr)     :<: instr
       , Monad m
       )
    => IExp instr Bool
    -> ProgramT instr m (IExp instr a)
    -> ProgramT instr m (IExp instr a)
    -> ProgramT instr m (IExp instr a)
ifE b t f = do
    r <- newRef
    iff b (t >>= setRef r) (f >>= setRef r)
    getRef r

while :: (ControlCMD (IExp instr) :<: instr)
    => ProgramT instr m (IExp instr Bool)
    -> ProgramT instr m ()
    -> ProgramT instr m ()
while b t = singleE $ While b t

whileE
    :: ( VarPred (IExp instr) a
       , ControlCMD (IExp instr) :<: instr
       , RefCMD (IExp instr)     :<: instr
       , Monad m
       )
    => ProgramT instr m (IExp instr Bool)
    -> ProgramT instr m (IExp instr a)
    -> ProgramT instr m (IExp instr a)
whileE b t = do
    r <- newRef
    while b (t >>= setRef r)
    getRef r

break :: (ControlCMD (IExp instr) :<: instr) => ProgramT instr m ()
break = singleE Break

fopen :: (FileCMD (IExp instr) :<: instr) => FilePath -> IOMode -> ProgramT instr m Handle
fopen file = singleE . FOpen file

fclose :: (FileCMD (IExp instr) :<: instr) => Handle -> ProgramT instr m ()
fclose = singleE . FClose

feof :: (VarPred (IExp instr) Bool, FileCMD (IExp instr) :<: instr) =>
    Handle -> ProgramT instr m (IExp instr Bool)
feof = singleE . FEof

class PrintfType r
  where
    type PrintfExp r :: * -> *
    fprf :: Handle -> String -> [FunArg PrintfArg (PrintfExp r)] -> r

instance (FileCMD (IExp instr) :<: instr) => PrintfType (ProgramT instr m ())
  where
    type PrintfExp (ProgramT instr m ()) = IExp instr
    fprf h form as = singleE $ FPrintf h form (reverse as)

instance (PrintfArg a, PrintfType r, exp ~ PrintfExp r) => PrintfType (exp a -> r)
  where
    type PrintfExp (exp a -> r) = exp
    fprf h form as = \a -> fprf h form (FunArg a : as)

fprintf :: PrintfType r => Handle -> String -> r
fprintf h format = fprf h format []

fput :: (Show a, PrintfArg a, FileCMD (IExp instr) :<: instr) =>
    Handle -> IExp instr a -> ProgramT instr m ()
fput hdl a = fprintf hdl "%f" a

fget :: (Read a, Scannable a, VarPred (IExp instr) a, FileCMD (IExp instr) :<: instr) =>
    Handle -> ProgramT instr m (IExp instr a)
fget = singleE . FGet

printf :: PrintfType r => String -> r
printf = fprintf stdout

addInclude :: (CallCMD (IExp instr) :<: instr) => String -> ProgramT instr m ()
addInclude = singleE . AddInclude

addDefinition :: (CallCMD (IExp instr) :<: instr) => C.Definition -> ProgramT instr m ()
addDefinition = singleE . AddDefinition

callFun :: (VarPred (IExp instr) a, CallCMD (IExp instr) :<: instr)
    => String                     -- ^ Function name
    -> [FunArg Any (IExp instr)]  -- ^ Arguments
    -> ProgramT instr m (IExp instr a)
callFun fun as = singleE $ CallFun fun as

callProc :: (CallCMD (IExp instr) :<: instr)
    => String                     -- ^ Function name
    -> [FunArg Any (IExp instr)]  -- ^ Arguments
    -> ProgramT instr m ()
callProc fun as = singleE $ CallProc fun as

getTime :: (VarPred (IExp instr) Double, CallCMD (IExp instr) :<: instr, Monad m) =>
    ProgramT instr m (IExp instr Double)
getTime = do
    addInclude "<sys/time.h>"
    addInclude "<sys/resource.h>"
    addDefinition getTimeDef
    callFun "get_time" []
  where
    getTimeDef = [cedecl|
      double get_time()
      {
          struct timeval t;
          struct timezone tzp;
          gettimeofday(&t, &tzp);
          return t.tv_sec + t.tv_usec*1e-6;
      }
      |]
      -- From http://stackoverflow.com/questions/2349776/how-can-i-benchmark-c-code-easily

