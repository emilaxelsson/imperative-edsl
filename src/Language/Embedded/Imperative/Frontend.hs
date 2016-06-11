{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

-- Front end for imperative programs

module Language.Embedded.Imperative.Frontend where



import Prelude hiding (break)

import Data.Int (Int32)
import Data.Array.IO
import Data.IORef
import Data.Typeable
import System.IO.Unsafe

import Control.Monad.Operational.Higher
import System.IO.Fake
import Language.Embedded.Expression
import Language.Embedded.Imperative.CMD
import Language.Embedded.Imperative.Args
import Language.Embedded.Imperative.Frontend.General



--------------------------------------------------------------------------------
-- * References
--------------------------------------------------------------------------------

-- | Create an uninitialized reference
newRef :: (pred a, RefCMD :<: instr) =>
    ProgramT instr (Param2 exp pred) m (Ref a)
newRef = newNamedRef "r"

-- | Create an uninitialized named reference
--
-- The provided base name may be appended with a unique identifier to avoid name
-- collisions.
newNamedRef :: (pred a, RefCMD :<: instr)
    => String  -- ^ Base name
    -> ProgramT instr (Param2 exp pred) m (Ref a)
newNamedRef = singleInj . NewRef

-- | Create an initialized reference
initRef :: (pred a, RefCMD :<: instr)
    => exp a  -- ^ Initial value
    -> ProgramT instr (Param2 exp pred) m (Ref a)
initRef = initNamedRef "r"

-- | Create an initialized named reference
--
-- The provided base name may be appended with a unique identifier to avoid name
-- collisions.
initNamedRef :: (pred a, RefCMD :<: instr)
    => String  -- ^ Base name
    -> exp a   -- ^ Initial value
    -> ProgramT instr (Param2 exp pred) m (Ref a)
initNamedRef base a = singleInj (InitRef base a)

-- | Get the contents of a reference
getRef :: (pred a, FreeExp exp, FreePred exp a, RefCMD :<: instr, Monad m) =>
    Ref a -> ProgramT instr (Param2 exp pred) m (exp a)
getRef = fmap valToExp . singleInj . GetRef

-- | Set the contents of a reference
setRef :: (pred a, RefCMD :<: instr) =>
    Ref a -> exp a -> ProgramT instr (Param2 exp pred) m ()
setRef r = singleInj . SetRef r

-- | Modify the contents of reference
modifyRef :: (pred a, FreeExp exp, FreePred exp a, RefCMD :<: instr, Monad m) =>
    Ref a -> (exp a -> exp a) -> ProgramT instr (Param2 exp pred) m ()
modifyRef r f = setRef r . f =<< unsafeFreezeRef r

-- | Freeze the contents of reference (only safe if the reference is not updated
-- as long as the resulting value is alive)
unsafeFreezeRef
    :: (pred a, FreeExp exp, FreePred exp a, RefCMD :<: instr, Monad m)
    => Ref a -> ProgramT instr (Param2 exp pred) m (exp a)
unsafeFreezeRef = fmap valToExp . singleInj . UnsafeFreezeRef

-- | Read the value of a reference without returning in the monad
--
-- WARNING: Don't use this function unless you really know what you are doing.
-- It is almost always better to use 'unsafeFreezeRef' instead.
--
-- 'veryUnsafeFreezeRef' behaves predictably when doing code generation, but it
-- can give strange results when running in 'IO', as explained here:
--
-- <http://fun-discoveries.blogspot.se/2015/09/strictness-can-fix-non-termination.html>
veryUnsafeFreezeRef :: (FreeExp exp, FreePred exp a) => Ref a -> exp a
veryUnsafeFreezeRef (RefRun r)  = constExp $! unsafePerformIO $! readIORef r
veryUnsafeFreezeRef (RefComp v) = varExp v



--------------------------------------------------------------------------------
-- * Arrays
--------------------------------------------------------------------------------

-- | Create an uninitialized array
newArr :: (pred a, Integral i, Ix i, ArrCMD :<: instr)
    => exp i  -- ^ Length
    -> ProgramT instr (Param2 exp pred) m (Arr i a)
newArr = newNamedArr "a"

-- | Create an uninitialized named array
--
-- The provided base name may be appended with a unique identifier to avoid name
-- collisions.
newNamedArr :: (pred a, Integral i, Ix i, ArrCMD :<: instr)
    => String -- ^ Base name
    -> exp i  -- ^ Length
    -> ProgramT instr (Param2 exp pred) m (Arr i a)
newNamedArr base len = singleInj (NewArr base len)

-- | Create and initialize an array
initArr :: (pred a, Integral i, Ix i, ArrCMD :<: instr)
    => [a]  -- ^ Initial contents
    -> ProgramT instr (Param2 exp pred) m (Arr i a)
initArr = initNamedArr "a"

-- | Create and initialize a named array
--
-- The provided base name may be appended with a unique identifier to avoid name
-- collisions.
initNamedArr :: (pred a, Integral i, Ix i, ArrCMD :<: instr)
    => String  -- ^ Base name
    -> [a]     -- ^ Initial contents
    -> ProgramT instr (Param2 exp pred) m (Arr i a)
initNamedArr base init = singleInj (InitArr base init)

-- | Get an element of an array
getArr
    :: ( pred a
       , FreeExp exp
       , FreePred exp a
       , Integral i
       , Ix i
       , ArrCMD :<: instr
       , Monad m
       )
    => exp i -> Arr i a -> ProgramT instr (Param2 exp pred) m (exp a)
getArr i arr = fmap valToExp $ singleInj $ GetArr i arr

-- | Set an element of an array
setArr :: (pred a, Integral i, Ix i, ArrCMD :<: instr) =>
    exp i -> exp a -> Arr i a -> ProgramT instr (Param2 exp pred) m ()
setArr i a arr = singleInj (SetArr i a arr)

-- | Copy the contents of an array to another array. The number of elements to
-- copy must not be greater than the number of allocated elements in either
-- array.
copyArr :: (pred a, Integral i, Ix i, ArrCMD :<: instr)
    => Arr i a  -- ^ Destination
    -> Arr i a  -- ^ Source
    -> exp i    -- ^ Number of elements
    -> ProgramT instr (Param2 exp pred) m ()
copyArr arr1 arr2 len = singleInj $ CopyArr arr1 arr2 len

-- | Freeze a mutable array to an immutable one. This involves copying the array
-- to a newly allocated one.
freezeArr :: (pred a, Integral i, Ix i, ArrCMD :<: instr, Monad m)
    => Arr i a
    -> exp i  -- ^ Length of new array
    -> ProgramT instr (Param2 exp pred) m (IArr i a)
freezeArr arr n = do
    arr2 <- newArr n
    copyArr arr2 arr n
    unsafeFreezeArr arr2

-- | Freeze a mutable array to an immutable one without making a copy. This is
-- generally only safe if the the mutable array is not updated as long as the
-- immutable array is alive.
unsafeFreezeArr :: (pred a, Integral i, Ix i, ArrCMD :<: instr) =>
    Arr i a -> ProgramT instr (Param2 exp pred) m (IArr i a)
unsafeFreezeArr arr = singleInj $ UnsafeFreezeArr arr

-- | Thaw an immutable array to a mutable one. This involves copying the array
-- to a newly allocated one.
thawArr :: (pred a, Integral i, Ix i, ArrCMD :<: instr, Monad m)
    => IArr i a
    -> exp i  -- ^ Number of elements to copy
    -> ProgramT instr (Param2 exp pred) m (Arr i a)
thawArr arr n = do
    arr2 <- unsafeThawArr arr
    arr3 <- newArr n
    copyArr arr3 arr2 n
    return arr3

-- | Thaw an immutable array to a mutable one without making a copy. This is
-- generally only safe if the the mutable array is not updated as long as the
-- immutable array is alive.
unsafeThawArr :: (pred a, Integral i, Ix i, ArrCMD :<: instr) =>
    IArr i a -> ProgramT instr (Param2 exp pred) m (Arr i a)
unsafeThawArr arr = singleInj $ UnsafeThawArr arr

-- | Create and initialize an immutable array
initIArr :: (pred a, Integral i, Ix i, ArrCMD :<: instr, Monad m) =>
    [a] -> ProgramT instr (Param2 exp pred) m (IArr i a)
initIArr = unsafeFreezeArr <=< initArr



--------------------------------------------------------------------------------
-- * Control flow
--------------------------------------------------------------------------------

-- | Conditional statement
iff :: (ControlCMD :<: instr)
    => exp Bool      -- ^ Condition
    -> ProgramT instr (Param2 exp pred) m ()  -- ^ True branch
    -> ProgramT instr (Param2 exp pred) m ()  -- ^ False branch
    -> ProgramT instr (Param2 exp pred) m ()
iff b t f = singleInj $ If b t f

-- | Conditional statement that returns an expression
ifE
    :: ( pred a
       , FreeExp exp
       , FreePred exp a
       , ControlCMD :<: instr
       , RefCMD     :<: instr
       , Monad m
       )
    => exp Bool                                    -- ^ Condition
    -> ProgramT instr (Param2 exp pred) m (exp a)  -- ^ True branch
    -> ProgramT instr (Param2 exp pred) m (exp a)  -- ^ False branch
    -> ProgramT instr (Param2 exp pred) m (exp a)
ifE b t f = do
    r <- newRef
    iff b (t >>= setRef r) (f >>= setRef r)
    getRef r

-- | While loop
while :: (ControlCMD :<: instr)
    => ProgramT instr (Param2 exp pred) m (exp Bool)  -- ^ Continue condition
    -> ProgramT instr (Param2 exp pred) m ()          -- ^ Loop body
    -> ProgramT instr (Param2 exp pred) m ()
while b t = singleInj $ While b t

-- | For loop
for
    :: ( FreeExp exp
       , ControlCMD :<: instr
       , Integral n
       , pred n
       , FreePred exp n
       )
    => IxRange (exp n)                                   -- ^ Index range
    -> (exp n -> ProgramT instr (Param2 exp pred) m ())  -- ^ Loop body
    -> ProgramT instr (Param2 exp pred) m ()
for range body = singleInj $ For range (body . valToExp)

-- | Break out from a loop
break :: (ControlCMD :<: instr) => ProgramT instr (Param2 exp pred) m ()
break = singleInj Break

-- | Assertion
assert :: (ControlCMD :<: instr)
    => exp Bool  -- ^ Expression that should be true
    -> String    -- ^ Message in case of failure
    -> ProgramT instr (Param2 exp pred) m ()
assert cond msg = singleInj $ Assert cond msg



--------------------------------------------------------------------------------
-- * Pointer operations
--------------------------------------------------------------------------------

-- | Swap two pointers
--
-- This is generally an unsafe operation. E.g. it can be used to make a
-- reference to a data structure escape the scope of the data.
--
-- The 'IsPointer' class ensures that the operation is only possible for types
-- that are represented as pointers in C.
unsafeSwap :: (IsPointer a, PtrCMD :<: instr) =>
    a -> a -> ProgramT instr (Param2 exp pred) m ()
unsafeSwap a b = singleInj $ SwapPtr a b



--------------------------------------------------------------------------------
-- * File handling
--------------------------------------------------------------------------------

-- | Open a file
fopen :: (FileCMD :<: instr) =>
    FilePath -> IOMode -> ProgramT instr (Param2 exp pred) m Handle
fopen file = singleInj . FOpen file

-- | Close a file
fclose :: (FileCMD :<: instr) => Handle -> ProgramT instr (Param2 exp pred) m ()
fclose = singleInj . FClose

-- | Check for end of file
feof :: (FreeExp exp, FreePred exp Bool, FileCMD :<: instr, Monad m) =>
    Handle -> ProgramT instr (Param2 exp pred) m (exp Bool)
feof = fmap valToExp . singleInj . FEof

class PrintfType r
  where
    type PrintfExp r :: * -> *
    fprf :: Handle -> String -> [PrintfArg (PrintfExp r)] -> r

instance (FileCMD :<: instr, a ~ ()) =>
    PrintfType (ProgramT instr (Param2 exp pred) m a)
  where
    type PrintfExp (ProgramT instr (Param2 exp pred) m a) = exp
    fprf h form as = singleInj $ FPrintf h form (reverse as)

instance (Formattable a, PrintfType r, exp ~ PrintfExp r) =>
    PrintfType (exp a -> r)
  where
    type PrintfExp  (exp a -> r) = exp
    fprf h form as = \a -> fprf h form (PrintfArg a : as)

-- | Print to a handle. Accepts a variable number of arguments.
fprintf :: PrintfType r => Handle -> String -> r
fprintf h format = fprf h format []

-- | Put a single value to a handle
fput :: forall instr exp pred a m
    .  (Formattable a, FreePred exp a, FileCMD :<: instr)
    => Handle
    -> String  -- ^ Prefix
    -> exp a   -- ^ Expression to print
    -> String  -- ^ Suffix
    -> ProgramT instr (Param2 exp pred) m ()
fput hdl prefix a suffix =
    fprintf hdl (prefix ++ formatSpecifier (Proxy :: Proxy a) ++ suffix) a

-- | Get a single value from a handle
fget
    :: ( Formattable a
       , pred a
       , FreeExp exp
       , FreePred exp a
       , FileCMD :<: instr
       , Monad m
       )
    => Handle -> ProgramT instr (Param2 exp pred) m (exp a)
fget = fmap valToExp . singleInj . FGet

-- | Print to @stdout@. Accepts a variable number of arguments.
printf :: PrintfType r => String -> r
printf = fprintf stdout



--------------------------------------------------------------------------------
-- * C-specific commands
--------------------------------------------------------------------------------

-- | Create a null pointer
newPtr :: (pred a, C_CMD :<: instr) => ProgramT instr (Param2 exp pred) m (Ptr a)
newPtr = newNamedPtr "p"

-- | Create a named null pointer
--
-- The provided base name may be appended with a unique identifier to avoid name
-- collisions.
newNamedPtr :: (pred a, C_CMD :<: instr)
    => String  -- ^ Base name
    -> ProgramT instr (Param2 exp pred) m (Ptr a)
newNamedPtr = singleInj . NewPtr

-- | Cast a pointer to an array
ptrToArr :: (C_CMD :<: instr) => Ptr a -> ProgramT instr (Param2 exp pred) m (Arr i a)
ptrToArr = singleInj . PtrToArr

-- | Create a pointer to an abstract object. The only thing one can do with such
-- objects is to pass them to 'callFun' or 'callProc'.
newObject :: (C_CMD :<: instr)
    => String  -- ^ Object type
    -> Bool    -- ^ Pointed?
    -> ProgramT instr (Param2 exp pred) m Object
newObject t p = newNamedObject "obj" t p

-- | Create a pointer to a named abstract object. The only thing one can do with
-- such objects is to pass them to 'callFun' or 'callProc'.
--
-- The provided base name may be appended with a unique identifier to avoid name
-- collisions.
newNamedObject :: (C_CMD :<: instr)
    => String  -- ^ Base name
    -> String  -- ^ Object type
    -> Bool    -- ^ Pointed?
    -> ProgramT instr (Param2 exp pred) m Object
newNamedObject base t p = singleInj $ NewObject base t p

-- | Add an @#include@ statement to the generated code
addInclude :: (C_CMD :<: instr) => String -> ProgramT instr (Param2 exp pred) m ()
addInclude = singleInj . AddInclude

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
addDefinition :: (C_CMD :<: instr) => Definition -> ProgramT instr (Param2 exp pred) m ()
addDefinition = singleInj . AddDefinition

-- | Declare an external function
addExternFun :: (pred res, C_CMD :<: instr)
    => String             -- ^ Function name
    -> proxy res          -- ^ Proxy for result type
    -> [FunArg exp pred]  -- ^ Arguments (only used to determine types)
    -> ProgramT instr (Param2 exp pred) m ()
addExternFun fun res args = singleInj $ AddExternFun fun res args

-- | Declare an external procedure
addExternProc :: (C_CMD :<: instr)
    => String             -- ^ Procedure name
    -> [FunArg exp pred]  -- ^ Arguments (only used to determine types)
    -> ProgramT instr (Param2 exp pred) m ()
addExternProc proc args = singleInj $ AddExternProc proc args

-- | Call a function
callFun :: (pred a, FreeExp exp, FreePred exp a, C_CMD :<: instr, Monad m)
    => String             -- ^ Function name
    -> [FunArg exp pred]  -- ^ Arguments
    -> ProgramT instr (Param2 exp pred) m (exp a)
callFun fun as = fmap valToExp $ singleInj $ CallFun fun as

-- | Call a procedure
callProc :: (C_CMD :<: instr)
    => String             -- ^ Procedure name
    -> [FunArg exp pred]  -- ^ Arguments
    -> ProgramT instr (Param2 exp pred) m ()
callProc fun as = singleInj $ CallProc (Nothing :: Maybe Object) fun as

-- | Call a procedure and assign its result
callProcAssign :: (Assignable obj, C_CMD :<: instr)
    => obj                -- ^ Object to which the result should be assigned
    -> String             -- ^ Procedure name
    -> [FunArg exp pred]  -- ^ Arguments
    -> ProgramT instr (Param2 exp pred) m ()
callProcAssign obj fun as = singleInj $ CallProc (Just obj) fun as
  -- The reason for having both `callProc` and `callProcAssign` instead of a
  -- single one with a `Maybe obj` is that the caller would have to resolve the
  -- overloading when passing `Nothing` (as currently done in `callProc`).

-- | Declare and call an external function
externFun :: forall instr m exp pred res
    .  (pred res, FreeExp exp, FreePred exp res, C_CMD :<: instr, Monad m)
    => String             -- ^ Function name
    -> [FunArg exp pred]  -- ^ Arguments
    -> ProgramT instr (Param2 exp pred) m (exp res)
externFun fun args = do
    addExternFun fun (Proxy :: Proxy res) args
    callFun fun args

-- | Declare and call an external procedure
externProc :: (C_CMD :<: instr, Monad m)
    => String        -- ^ Procedure name
    -> [FunArg exp pred]  -- ^ Arguments
    -> ProgramT instr (Param2 exp pred) m ()
externProc proc args = do
    addExternProc proc args
    callProc proc args

-- | Generate code into another translation unit
inModule :: (C_CMD :<: instr)
    => String
    -> ProgramT instr (Param2 exp pred) m ()
    -> ProgramT instr (Param2 exp pred) m ()
inModule mod prog = singleInj $ InModule mod prog

-- | Get current time as number of seconds passed today
getTime
    :: (pred Double, FreeExp exp, FreePred exp Double, C_CMD :<: instr, Monad m)
    => ProgramT instr (Param2 exp pred) m (exp Double)
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

-- ...

type Offset = Int

offload :: (C_CMD :<: instr) => String -> ProgramT instr (Param2 exp pred) m (Ptr Int32)
offload = singleInj . Offload

assignp :: (C_CMD :<: instr) => Ptr Int32 -> Offset -> exp Int32 -> ProgramT instr (Param2 exp pred) m ()
assignp ptr o = singleInj . AssignPtr ptr o

loadp   :: (pred Int32, C_CMD :<: instr, FreeExp exp, FreePred exp Int32, Monad m)
        => Ptr Int32 -> Offset -> ProgramT instr (Param2 exp pred) m (exp Int32)
loadp ptr o = fmap valToExp $ singleInj $ LoadPtr ptr o

closep  :: (C_CMD :<: instr) => ProgramT instr (Param2 exp pred) m ()
closep = singleInj $ ClosePtr

-- Arguments

-- | Value argument
valArg :: pred a => exp a -> FunArg exp pred
valArg = ValArg

-- | Reference argument
refArg :: (pred a, Arg RefArg pred) => Ref a -> FunArg exp pred
refArg = FunArg . RefArg

-- | Mutable array argument
arrArg :: (pred a, Arg ArrArg pred) => Arr i a -> FunArg exp pred
arrArg = FunArg . ArrArg

-- | Immutable array argument
iarrArg :: (pred a, Arg IArrArg pred) => IArr i a -> FunArg exp pred
iarrArg = FunArg . IArrArg

-- | Pointer argument
ptrArg :: (pred a, Arg PtrArg pred) => Ptr a -> FunArg exp pred
ptrArg = FunArg . PtrArg

-- | Abstract object argument
objArg :: Object -> FunArg exp pred
objArg = FunArg . ObjArg

-- | Constant string argument
strArg :: String -> FunArg exp pred
strArg = FunArg . StrArg

-- | Modifier that takes the address of another argument
addr :: FunArg exp pred -> FunArg exp pred
addr = AddrArg

-- | Modifier that dereferences another argument
deref :: FunArg exp pred -> FunArg exp pred
deref = DerefArg



--------------------------------------------------------------------------------
-- * Running programs
--------------------------------------------------------------------------------

-- | Run a program in 'IO'. Note that not all instructions are supported for
-- running in 'IO'. For example, calls to external C functions are not
-- supported.
runIO :: (EvalExp exp, InterpBi instr IO (Param1 pred), HBifunctor instr) =>
    Program instr (Param2 exp pred) a -> IO a
runIO = interpretBi (return . evalExp)

-- | Like 'runIO' but with explicit input/output connected to @stdin@/@stdout@
captureIO :: (EvalExp exp, InterpBi instr IO (Param1 pred), HBifunctor instr)
    => Program instr (Param2 exp pred) a  -- ^ Program to run
    -> String                             -- ^ Input to send to @stdin@
    -> IO String                          -- ^ Result from @stdout@
captureIO = fakeIO . runIO

