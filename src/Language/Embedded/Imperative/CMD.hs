{-# LANGUAGE CPP #-}

module Language.Embedded.Imperative.CMD where



import Data.Array.IO
import Data.Char (isSpace)
import Data.Int
import Data.IORef
import Data.Typeable
import Data.Word
import System.IO (IOMode (..))
import qualified System.IO as IO
import Text.Printf (PrintfArg)
import qualified Text.Printf as Printf

import Control.Monad.Operational.Compositional
import Data.TypePredicates
import Language.Embedded.Expression
import qualified Language.C.Syntax as C



--------------------------------------------------------------------------------
-- * References
--------------------------------------------------------------------------------

data Ref a
    = RefComp VarId
    | RefEval (IORef a)
  deriving Typeable

-- | Commands for mutable references
data RefCMD exp (prog :: * -> *) a
  where
    NewRef  :: VarPred exp a => RefCMD exp prog (Ref a)
    InitRef :: VarPred exp a => exp a -> RefCMD exp prog (Ref a)
    GetRef  :: VarPred exp a => Ref a -> RefCMD exp prog (exp a)
    SetRef  :: VarPred exp a => Ref a -> exp a -> RefCMD exp prog ()
      -- `VarPred` for `SetRef` is not needed for code generation, but it can be useful when
      -- interpreting with a dynamically typed store. `VarPred` can then be used to supply a
      -- `Typeable` dictionary for casting.
#if  __GLASGOW_HASKELL__>=708
  deriving Typeable
#endif

instance MapInstr (RefCMD exp)
  where
    imap _ NewRef              = NewRef
    imap _ (InitRef a)         = InitRef a
    imap _ (GetRef r)          = GetRef r
    imap _ (SetRef r a)        = SetRef r a

instance CompExp exp => DryInterp (RefCMD exp)
  where
    dryInterp NewRef       = liftM RefComp fresh
    dryInterp (InitRef _)  = liftM RefComp fresh
    dryInterp (GetRef _)   = liftM varExp fresh
    dryInterp (SetRef _ _) = return ()

type instance IExp (RefCMD e)       = e
type instance IExp (RefCMD e :+: i) = e



--------------------------------------------------------------------------------
-- * Arrays
--------------------------------------------------------------------------------

data Arr n a
    = ArrComp String
    | ArrEval (IOArray n a)
  deriving Typeable

-- | Commands for mutable arrays
data ArrCMD exp (prog :: * -> *) a
  where
    NewArr :: (VarPred exp a, VarPred exp n, Integral n, Ix n) => exp n -> exp a -> ArrCMD exp prog (Arr n a)
    GetArr :: (VarPred exp a, Integral n, Ix n)                => exp n -> Arr n a -> ArrCMD exp prog (exp a)
    SetArr :: (Integral n, Ix n)                               => exp n -> exp a -> Arr n a -> ArrCMD exp prog ()
#if  __GLASGOW_HASKELL__>=708
  deriving Typeable
#endif

instance MapInstr (ArrCMD exp)
  where
    imap _ (NewArr n a)     = NewArr n a
    imap _ (GetArr i arr)   = GetArr i arr
    imap _ (SetArr i a arr) = SetArr i a arr

instance CompExp exp => DryInterp (ArrCMD exp)
  where
    dryInterp (NewArr _ _)   = liftM ArrComp $ freshStr "a"
    dryInterp (GetArr _ _)   = liftM varExp fresh
    dryInterp (SetArr _ _ _) = return ()

type instance IExp (ArrCMD e)       = e
type instance IExp (ArrCMD e :+: i) = e



--------------------------------------------------------------------------------
-- * Control flow
--------------------------------------------------------------------------------

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

instance DryInterp (ControlCMD exp)
  where
    dryInterp (If _ _ _)  = return ()
    dryInterp (While _ _) = return ()
    dryInterp Break       = return ()

type instance IExp (ControlCMD e)       = e
type instance IExp (ControlCMD e :+: i) = e



--------------------------------------------------------------------------------
-- * File handling
--------------------------------------------------------------------------------

data Handle
    = HandleComp String
    | HandleEval IO.Handle
  deriving Typeable

class Typeable a => Scannable a
  where
    scanFormatSpecifier :: Proxy a -> String

instance Scannable Int     where scanFormatSpecifier _ = "%d"
instance Scannable Int8    where scanFormatSpecifier _ = "%d"
instance Scannable Int16   where scanFormatSpecifier _ = "%d"
instance Scannable Int32   where scanFormatSpecifier _ = "%d"
instance Scannable Int64   where scanFormatSpecifier _ = "%d"
instance Scannable Word    where scanFormatSpecifier _ = "%d"
instance Scannable Word8   where scanFormatSpecifier _ = "%d"
instance Scannable Word16  where scanFormatSpecifier _ = "%d"
instance Scannable Word32  where scanFormatSpecifier _ = "%d"
instance Scannable Word64  where scanFormatSpecifier _ = "%d"
instance Scannable Float   where scanFormatSpecifier _ = "%f"
instance Scannable Double  where scanFormatSpecifier _ = "%f"

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

instance CompExp exp => DryInterp (FileCMD exp)
  where
    dryInterp (FOpen _ _)     = liftM HandleComp $ freshStr "h"
    dryInterp (FClose _)      = return ()
    dryInterp (FPrintf _ _ _) = return ()
    dryInterp (FGet _)        = liftM varExp fresh
    dryInterp (FEof _)        = liftM varExp fresh

type instance IExp (FileCMD e)       = e
type instance IExp (FileCMD e :+: i) = e



--------------------------------------------------------------------------------
-- * External function calls
--------------------------------------------------------------------------------

-- | A function argument with constrained existentially quantified type
data FunArg pred exp
  where
    ValArg :: pred a               => exp a -> FunArg pred exp
    RefArg :: (pred a, Typeable a) => Ref a -> FunArg pred exp

-- | Cast the argument predicate to 'Any'
anyArg :: FunArg pred exp -> FunArg Any exp
anyArg (ValArg a) = ValArg a
anyArg (RefArg r) = RefArg r

data CallCMD exp (prog :: * -> *) a
  where
    AddInclude    :: String       -> CallCMD exp prog ()
    AddDefinition :: C.Definition -> CallCMD exp prog ()
    AddExternFun  :: VarPred exp res
                  => String
                  -> proxy (exp res)
                  -> [FunArg (VarPred exp) exp]
                  -> CallCMD exp prog ()
    AddExternProc :: String -> [FunArg (VarPred exp) exp] -> CallCMD exp prog ()
    CallFun       :: VarPred exp a => String -> [FunArg Any exp] -> CallCMD exp prog (exp a)
    CallProc      ::                  String -> [FunArg Any exp] -> CallCMD exp prog ()

instance MapInstr (CallCMD exp)
  where
    imap _ (AddInclude incl)           = AddInclude incl
    imap _ (AddDefinition def)         = AddDefinition def
    imap _ (AddExternFun fun res args) = AddExternFun fun res args
    imap _ (AddExternProc proc args)   = AddExternProc proc args
    imap _ (CallFun fun args)          = CallFun fun args
    imap _ (CallProc proc args)        = CallProc proc args

instance CompExp exp => DryInterp (CallCMD exp)
  where
    dryInterp (AddInclude _)       = return ()
    dryInterp (AddDefinition _)    = return ()
    dryInterp (AddExternFun _ _ _) = return ()
    dryInterp (AddExternProc _ _)  = return ()
    dryInterp (CallFun _ _)        = liftM varExp fresh
    dryInterp (CallProc _ _)       = return ()

type instance IExp (CallCMD e)       = e
type instance IExp (CallCMD e :+: i) = e



--------------------------------------------------------------------------------
-- * Running commands
--------------------------------------------------------------------------------

runRefCMD :: forall exp prog a . EvalExp exp => RefCMD exp prog a -> IO a
runRefCMD (InitRef a)                       = fmap RefEval $ newIORef $ evalExp a
runRefCMD NewRef                            = fmap RefEval $ newIORef $ error "reading uninitialized reference"
runRefCMD (SetRef (RefEval r) a)            = writeIORef r $ evalExp a
runRefCMD (GetRef (RefEval (r :: IORef b))) = fmap litExp $ readIORef r

runArrCMD :: EvalExp exp => ArrCMD exp prog a -> IO a
runArrCMD (NewArr n a) =
    fmap ArrEval $ newArray (0, fromIntegral (evalExp n) - 1) (evalExp a)
runArrCMD (SetArr i a (ArrEval arr)) =
    writeArray arr (fromIntegral (evalExp i)) (evalExp a)
runArrCMD (GetArr i (ArrEval arr)) =
    fmap litExp $ readArray arr (fromIntegral (evalExp i))

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
evalFPrintf (ValArg a:as) pf = evalFPrintf as (pf $ evalExp a)

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
runCallCMD (AddInclude _)       = return ()
runCallCMD (AddDefinition _)    = return ()
runCallCMD (AddExternFun _ _ _) = return ()
runCallCMD (AddExternProc _ _)  = return ()
runCallCMD (CallFun _ _)        = error "cannot run programs involving callFun"
runCallCMD (CallProc _ _)       = error "cannot run programs involving callProc"

instance EvalExp exp => Interp (RefCMD exp)     IO where interp = runRefCMD
instance EvalExp exp => Interp (ArrCMD exp)     IO where interp = runArrCMD
instance EvalExp exp => Interp (ControlCMD exp) IO where interp = runControlCMD
instance EvalExp exp => Interp (FileCMD exp)    IO where interp = runFileCMD
instance EvalExp exp => Interp (CallCMD exp)    IO where interp = runCallCMD

