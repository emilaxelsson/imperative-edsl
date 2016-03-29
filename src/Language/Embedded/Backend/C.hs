{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

-- | C code generation for 'Program'

module Language.Embedded.Backend.C
  ( module Language.Embedded.Backend.C.Expression
  , module Language.Embedded.Backend.C
  ) where



#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
import Data.Monoid
#endif
import Control.Exception
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import System.Directory (getTemporaryDirectory, removeFile)
import System.Exit (ExitCode (..))
import System.IO
import System.Process (system)

import Data.Loc (noLoc)
import qualified Language.C.Syntax as C
import Text.PrettyPrint.Mainland (pretty)

import Control.Monad.Operational.Higher
import System.IO.Fake
import Language.C.Monad

import Language.Embedded.Backend.C.Expression



--------------------------------------------------------------------------------
-- * Utilities
--------------------------------------------------------------------------------

-- | Create a named type
namedType :: String -> C.Type
namedType t = C.Type
    (C.DeclSpec [] [] (C.Tnamed (C.Id t noLoc) [] noLoc) noLoc)
    (C.DeclRoot noLoc)
    noLoc

-- | Return the argument of a boolean negation expression
viewNotExp :: C.Exp -> Maybe C.Exp
viewNotExp (C.UnOp C.Lnot a _)                     = Just a
viewNotExp (C.FnCall (C.Var (C.Id "!" _) _) [a] _) = Just a
  -- Apparently this is what `!` parses to
viewNotExp _ = Nothing

arrayInit :: [C.Exp] -> C.Initializer
arrayInit as = C.CompoundInitializer
    [(Nothing, C.ExpInitializer a noLoc) | a <- as]
    noLoc



--------------------------------------------------------------------------------
-- * Code generation user interface
--------------------------------------------------------------------------------

-- | Compile a program to C code represented as a string
--
-- This function returns only the first (main) module.
-- To get every C translation units, use `compileAll`.
--
-- For programs that make use of the primitives in
-- "Language.Embedded.Concurrent", the resulting C code can be compiled as
-- follows:
--
-- > gcc -Iinclude csrc/chan.c -lpthread YOURPROGRAM.c
compile :: (Interp instr CGen (Param2 exp pred), HFunctor instr) =>
    Program instr (Param2 exp pred) a -> String
compile = snd . head . compileAll

compileAll :: (Interp instr CGen (Param2 exp pred), HFunctor instr) =>
    Program instr (Param2 exp pred) a -> [(String, String)]
compileAll = map (("", pretty 80) <*>) . prettyCGen . liftSharedLocals . wrapMain . interpret

-- | Compile a program to C code and print it on the screen
--
-- This function returns only the first (main) module.
-- To get every C translation units, use `icompileAll`.
--
-- For programs that make use of the primitives in
-- "Language.Embedded.Concurrent", the resulting C code can be compiled as
-- follows:
--
-- > gcc -Iinclude csrc/chan.c -lpthread YOURPROGRAM.c
icompile :: (Interp instr CGen (Param2 exp pred), HFunctor instr) =>
    Program instr (Param2 exp pred) a -> IO ()
icompile = putStrLn . compile

icompileAll :: (Interp instr CGen (Param2 exp pred), HFunctor instr) =>
    Program instr (Param2 exp pred) a -> IO ()
icompileAll = mapM_ (\(n, m) -> putStrLn ("// module " ++ n) >> putStrLn m) . compileAll

removeFileIfPossible :: FilePath -> IO ()
removeFileIfPossible file =
    catch (removeFile file) (\(_ :: SomeException) -> return ())

data ExternalCompilerOpts = ExternalCompilerOpts
      { externalKeepFiles  :: Bool      -- ^ Keep generated files?
      , externalFlagsPre   :: [String]  -- ^ External compiler flags (e.g. @["-Ipath"]@)
      , externalFlagsPost  :: [String]  -- ^ External compiler flags after C source (e.g. @["-lm","-lpthread"]@)
      , externalSilent     :: Bool      -- ^ Don't print anything besides what the program prints
      }

defaultExtCompilerOpts :: ExternalCompilerOpts
defaultExtCompilerOpts = ExternalCompilerOpts
    { externalKeepFiles = False
    , externalFlagsPre  = []
    , externalFlagsPost = []
    , externalSilent    = False
    }

instance Monoid ExternalCompilerOpts
  where
    mempty = defaultExtCompilerOpts
    mappend
        (ExternalCompilerOpts keep1 pre1 post1 silent1)
        (ExternalCompilerOpts keep2 pre2 post2 silent2) =
            ExternalCompilerOpts keep2 (pre1 ++ pre2) (post1 ++ post2) silent2

maybePutStrLn :: Bool -> String -> IO ()
maybePutStrLn False str = putStrLn str
maybePutStrLn _ _ = return ()

-- TODO: it would be nice to have a version that compiles all modules of a program,
-- as it currently compiles only the first (main) module.
-- | Generate C code and use GCC to compile it
compileC :: (Interp instr CGen (Param2 exp pred), HFunctor instr)
    => ExternalCompilerOpts
    -> Program instr (Param2 exp pred) a  -- ^ Program to compile
    -> IO FilePath                        -- ^ Path to the generated executable
compileC (ExternalCompilerOpts {..}) prog = do
    tmp <- getTemporaryDirectory
    t   <- fmap (formatTime defaultTimeLocale format) getCurrentTime
    (exeFile,exeh) <- openTempFile tmp ("edsl_" ++ t)
    hClose exeh
    let cFile = exeFile ++ ".c"
    writeFile cFile $ compile prog
    when externalKeepFiles $ maybePutStrLn externalSilent $
        "Created temporary file: " ++ cFile
    let compileCMD = unwords
          $  ["gcc", "-std=c99"]
          ++ externalFlagsPre
          ++ [cFile, "-o", exeFile]
          ++ externalFlagsPost
    maybePutStrLn externalSilent compileCMD
    exit <- system compileCMD
    unless externalKeepFiles $ removeFileIfPossible cFile
    case exit of
      ExitSuccess -> return exeFile
      err -> do removeFileIfPossible exeFile
                error "compileC: failed to compile generated C code"
  where
    format = if externalKeepFiles then "%a-%H-%M-%S_" else ""

-- | Generate C code and use GCC to check that it compiles (no linking)
compileAndCheck' :: (Interp instr CGen (Param2 exp pred), HFunctor instr) =>
    ExternalCompilerOpts -> Program instr (Param2 exp pred) a -> IO ()
compileAndCheck' opts prog = do
    let opts' = opts {externalFlagsPre = "-c" : externalFlagsPre opts}
    exe <- compileC opts' prog
    removeFileIfPossible exe

-- | Generate C code and use GCC to check that it compiles (no linking)
compileAndCheck :: (Interp instr CGen (Param2 exp pred), HFunctor instr) =>
    Program instr (Param2 exp pred) a -> IO ()
compileAndCheck = compileAndCheck' mempty

-- | Generate C code, use GCC to compile it, and run the resulting executable
runCompiled' :: (Interp instr CGen (Param2 exp pred), HFunctor instr) =>
    ExternalCompilerOpts -> Program instr (Param2 exp pred) a -> IO ()
runCompiled' opts@(ExternalCompilerOpts {..}) prog = do
    exe <- compileC opts prog
    maybePutStrLn externalSilent ""
    maybePutStrLn externalSilent "#### Running:"
    system exe
    removeFileIfPossible exe
    return ()

-- | Generate C code, use GCC to compile it, and run the resulting executable
runCompiled :: (Interp instr CGen (Param2 exp pred), HFunctor instr) =>
    Program instr (Param2 exp pred) a -> IO ()
runCompiled = runCompiled' mempty

-- | Like 'runCompiled'' but with explicit input/output connected to
-- @stdin@/@stdout@
captureCompiled' :: (Interp instr CGen (Param2 exp pred), HFunctor instr)
    => ExternalCompilerOpts
    -> Program instr (Param2 exp pred) a  -- ^ Program to run
    -> String                             -- ^ Input to send to @stdin@
    -> IO String                          -- ^ Result from @stdout@
captureCompiled' opts prog inp = do
    exe <- compileC opts prog
    out <- fakeIO (system exe) inp
    removeFileIfPossible exe
    return out

-- | Like 'runCompiled' but with explicit input/output connected to
-- @stdin@/@stdout@
captureCompiled :: (Interp instr CGen (Param2 exp pred), HFunctor instr)
    => Program instr (Param2 exp pred) a  -- ^ Program to run
    -> String                             -- ^ Input to send to @stdin@
    -> IO String                          -- ^ Result from @stdout@
captureCompiled = captureCompiled' defaultExtCompilerOpts

-- | Compare the content written to @stdout@ from the reference program and from
-- running the compiled C code
compareCompiled' :: (Interp instr CGen (Param2 exp pred), HFunctor instr)
    => ExternalCompilerOpts
    -> Program instr (Param2 exp pred) a  -- ^ Program to run
    -> IO a                               -- ^ Reference program
    -> String                             -- ^ Input to send to @stdin@
    -> IO ()
compareCompiled' opts@(ExternalCompilerOpts {..}) prog ref inp = do
    maybePutStrLn externalSilent "#### Reference program:"
    outRef <- fakeIO ref inp
    maybePutStrLn externalSilent outRef
    maybePutStrLn externalSilent "#### runCompiled:"
    outComp <- captureCompiled' opts prog inp
    maybePutStrLn externalSilent outComp
    if outRef /= outComp
      then error "runCompiled differs from reference program"
      else maybePutStrLn externalSilent
             "  -- runCompiled is consistent with reference program\n\n\n\n"

-- | Compare the content written to @stdout@ from the reference program and from
-- running the compiled C code
compareCompiled :: (Interp instr CGen (Param2 exp pred), HFunctor instr)
    => Program instr (Param2 exp pred) a  -- ^ Program to run
    -> IO a                               -- ^ Reference program
    -> String                             -- ^ Input to send to @stdin@
    -> IO ()
compareCompiled = compareCompiled' defaultExtCompilerOpts

