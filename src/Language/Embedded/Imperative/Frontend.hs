{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A generic user interface for imperative programs. Programs are
-- parameterized by instruction sets that can be combined in a modular way; e.g:
--
-- @
-- type MyProg exp a = `Program` (`RefCMD` exp `:+:` `ArrCMD` exp) a
-- @
--
-- Also, instructions are parameterized on the expression language. In the above
-- example, @exp@ can be any type (of kind @* -> *@) that implements the
-- 'EvalExp' and 'CompExp' classes.

module Language.Embedded.Imperative.Frontend where



import Prelude hiding (break)

import Data.Array.IO
import Data.IORef
import Data.Typeable
import System.IO.Unsafe
import Text.Printf (PrintfArg)

import Language.C.Quote.C
import qualified Language.C.Syntax as C

import Data.TypePredicates
import Control.Monad.Operational.Compositional
import Language.Embedded.Expression
import Language.Embedded.Imperative.CMD
import Language.Embedded.Imperative.Types



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
newArr
    :: ( pred a
       , pred i
       , Integral i
       , Ix i
       , ArrCMD (IExp instr) :<: instr
       , pred ~ VarPred (IExp instr)
       )
    => IExp instr i -> IExp instr a -> ProgramT instr m (Arr i a)
newArr n a = singleE $ NewArr n a

-- | Set the contents of an array
getArr
    :: ( VarPred (IExp instr) a
       , ArrCMD (IExp instr) :<: instr
       , Integral i
       , Ix i
       )
    => IExp instr i -> Arr i a -> ProgramT instr m (IExp instr a)
getArr i arr = singleE (GetArr i arr)

-- | Set the contents of an array
setArr
    :: ( VarPred (IExp instr) a
       , ArrCMD (IExp instr) :<: instr
       , Integral i
       , Ix i
       )
    => IExp instr i -> IExp instr a -> Arr i a -> ProgramT instr m ()
setArr i a arr = singleE (SetArr i a arr)

-- | Conditional statement
iff :: (ControlCMD (IExp instr) :<: instr)
    => IExp instr Bool      -- ^ Condition
    -> ProgramT instr m ()  -- ^ True branch
    -> ProgramT instr m ()  -- ^ False branch
    -> ProgramT instr m ()
iff b t f = singleE $ If b t f

-- | Conditional statement that returns an expression
ifE
    :: ( VarPred (IExp instr) a
       , ControlCMD (IExp instr) :<: instr
       , RefCMD (IExp instr)     :<: instr
       , Monad m
       )
    => IExp instr Bool                  -- ^ Condition
    -> ProgramT instr m (IExp instr a)  -- ^ True branch
    -> ProgramT instr m (IExp instr a)  -- ^ False branch
    -> ProgramT instr m (IExp instr a)
ifE b t f = do
    r <- newRef
    iff b (t >>= setRef r) (f >>= setRef r)
    getRef r

-- | While loop
while :: (ControlCMD (IExp instr) :<: instr)
    => ProgramT instr m (IExp instr Bool)  -- ^ Continue condition
    -> ProgramT instr m ()                 -- ^ Loop body
    -> ProgramT instr m ()
while b t = singleE $ While b t

-- | While loop that returns an expression
whileE
    :: ( VarPred (IExp instr) a
       , ControlCMD (IExp instr) :<: instr
       , RefCMD (IExp instr)     :<: instr
       , Monad m
       )
    => ProgramT instr m (IExp instr Bool)  -- ^ Continue condition
    -> ProgramT instr m (IExp instr a)     -- ^ Loop body
    -> ProgramT instr m (IExp instr a)
whileE b t = do
    r <- newRef
    while b (t >>= setRef r)
    getRef r

-- | Break out from a loop
break :: (ControlCMD (IExp instr) :<: instr) => ProgramT instr m ()
break = singleE Break

stdin, stdout :: Handle
stdin  = HandleComp "stdin"
stdout = HandleComp "stdout"

-- | Open a file
fopen :: (FileCMD (IExp instr) :<: instr) => FilePath -> IOMode -> ProgramT instr m Handle
fopen file = singleE . FOpen file

-- | Close a file
fclose :: (FileCMD (IExp instr) :<: instr) => Handle -> ProgramT instr m ()
fclose = singleE . FClose

-- | Check for end of file
feof :: (VarPred (IExp instr) Bool, FileCMD (IExp instr) :<: instr) =>
    Handle -> ProgramT instr m (IExp instr Bool)
feof = singleE . FEof

class PrintfType r
  where
    type PrintfExp r :: * -> *
    fprf :: Handle -> String -> [FunArg PrintfArg (PrintfExp r)] -> r

instance (FileCMD (IExp instr) :<: instr, a ~ ()) => PrintfType (ProgramT instr m a)
  where
    type PrintfExp (ProgramT instr m a) = IExp instr
    fprf h form as = singleE $ FPrintf h form (reverse as)

instance (PrintfArg a, PrintfType r, exp ~ PrintfExp r) => PrintfType (exp a -> r)
  where
    type PrintfExp (exp a -> r) = exp
    fprf h form as = \a -> fprf h form (ValArg a : as)

-- | Print to a handle. Accepts a variable number of arguments.
fprintf :: PrintfType r => Handle -> String -> r
fprintf h format = fprf h format []

-- | Put a single value to a handle
fput :: forall instr a m
    .  (PrintfArg a, Scannable a, FileCMD (IExp instr) :<: instr)
    => Handle -> IExp instr a -> ProgramT instr m ()
fput hdl a = fprintf hdl (scanFormatSpecifier (Proxy :: Proxy a)) a

-- | Get a single value from a handle
fget
    :: ( Read a
       , Scannable a
       , VarPred (IExp instr) a
       , FileCMD (IExp instr) :<: instr
       )
    => Handle -> ProgramT instr m (IExp instr a)
fget = singleE . FGet

-- | Print to @stdout@. Accepts a variable number of arguments.
printf :: PrintfType r => String -> r
printf = fprintf stdout

-- | Add an @#include@ statement to the generated code
addInclude :: (CallCMD (IExp instr) :<: instr) => String -> ProgramT instr m ()
addInclude = singleE . AddInclude

-- | Add a global definition to the generated code
--
-- Can be used conveniently as follows:
--
-- > {-# LANGUAGE QuasiQuotes #-}
-- >
-- > import Language.Embedded.Imperative
-- > import Language.C.Quote.C
-- >
-- > prog = do
-- >     ...
-- >     addDefinition myCFunction
-- >     ...
-- >   where
-- >     myCFunction = [cedecl|
-- >       void my_C_function( ... )
-- >       {
-- >           // C code
-- >           // goes here
-- >       }
-- >       |]
addDefinition :: (CallCMD (IExp instr) :<: instr) => C.Definition -> ProgramT instr m ()
addDefinition = singleE . AddDefinition

-- | Declare an external function
addExternFun :: (VarPred exp res, CallCMD exp :<: instr, exp ~ IExp instr)
    => String                      -- ^ Function name
    -> proxy (exp res)             -- ^ Proxy for expression and result type
    -> [FunArg (VarPred exp) exp]  -- ^ Arguments (only used to determine types)
    -> ProgramT instr m ()
addExternFun fun res args = singleE $ AddExternFun fun res args

-- | Declare an external procedure
addExternProc :: (CallCMD exp :<: instr, exp ~ IExp instr)
    => String                      -- ^ Procedure name
    -> [FunArg (VarPred exp) exp]  -- ^ Arguments (only used to determine types)
    -> ProgramT instr m ()
addExternProc proc args = singleE $ AddExternProc proc args

-- | Call a function
callFun :: (VarPred (IExp instr) a, CallCMD (IExp instr) :<: instr)
    => String                     -- ^ Function name
    -> [FunArg Any (IExp instr)]  -- ^ Arguments
    -> ProgramT instr m (IExp instr a)
callFun fun as = singleE $ CallFun fun as

-- | Call a procedure
callProc :: (CallCMD (IExp instr) :<: instr)
    => String                     -- ^ Procedure name
    -> [FunArg Any (IExp instr)]  -- ^ Arguments
    -> ProgramT instr m ()
callProc fun as = singleE $ CallProc fun as

-- | Declare and call an external function
externFun :: forall instr m exp res
    .  (VarPred exp res, CallCMD exp :<: instr, exp ~ IExp instr, Monad m)
    => String                      -- ^ Function name
    -> [FunArg (VarPred exp) exp]  -- ^ Arguments
    -> ProgramT instr m (exp res)
externFun fun args = do
    addExternFun fun (Proxy :: Proxy (exp res)) args
    callFun fun $ map anyArg args

-- | Declare and call an external procedure
externProc :: (CallCMD exp :<: instr, exp ~ IExp instr, Monad m)
    => String                      -- ^ Procedure name
    -> [FunArg (VarPred exp) exp]  -- ^ Arguments
    -> ProgramT instr m ()
externProc proc args = do
    addExternProc proc args
    callProc proc $ map anyArg args

-- | Get current time as number of seconds passed today
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

