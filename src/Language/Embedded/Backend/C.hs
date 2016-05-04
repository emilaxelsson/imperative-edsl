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
import System.Process (system, readProcess)

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

-- | Compile a program to C code represented as a string. To compile the
-- resulting C code, use something like
--
-- > cc -std=c99 YOURPROGRAM.c
--
-- This function returns only the first (main) module. To get all C translation
-- unit, use 'compileAll'.
compile :: (Interp instr CGen (Param2 exp pred), HFunctor instr) =>
    Program instr (Param2 exp pred) a -> String
compile = snd . head . compileAll

-- | Compile a program to C modules, each one represented as a pair of a name
-- and the code represented as a string. To compile the resulting C code, use
-- something like
--
-- > cc -std=c99 YOURPROGRAM.c
compileAll :: (Interp instr CGen (Param2 exp pred), HFunctor instr) =>
    Program instr (Param2 exp pred) a -> [(String, String)]
compileAll
    = map (("", pretty 80) <*>) . prettyCGen . liftSharedLocals
    . wrapMain . interpret

-- | Compile a program to C code and print it on the screen. To compile the
-- resulting C code, use something like
--
-- > cc -std=c99 YOURPROGRAM.c
icompile :: (Interp instr CGen (Param2 exp pred), HFunctor instr) =>
    Program instr (Param2 exp pred) a -> IO ()
icompile prog = case compileAll prog of
    [m] -> putStrLn $ snd m
    ms  -> mapM_ (\(n, m) -> putStrLn ("// module " ++ n) >> putStrLn m) ms

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

-- | Generate C code and use CC to compile it
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
          $  ["cc", "-std=c99"]
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

-- | Generate C code and use CC to check that it compiles (no linking)
compileAndCheck' :: (Interp instr CGen (Param2 exp pred), HFunctor instr) =>
    ExternalCompilerOpts -> Program instr (Param2 exp pred) a -> IO ()
compileAndCheck' opts prog = do
    let opts' = opts {externalFlagsPre = "-c" : externalFlagsPre opts}
    exe <- compileC opts' prog
    removeFileIfPossible exe

-- | Generate C code and use CC to check that it compiles (no linking)
compileAndCheck :: (Interp instr CGen (Param2 exp pred), HFunctor instr) =>
    Program instr (Param2 exp pred) a -> IO ()
compileAndCheck = compileAndCheck' mempty

-- | Generate C code, use CC to compile it, and run the resulting executable
runCompiled' :: (Interp instr CGen (Param2 exp pred), HFunctor instr) =>
    ExternalCompilerOpts -> Program instr (Param2 exp pred) a -> IO ()
runCompiled' opts@(ExternalCompilerOpts {..}) prog = bracket
    (compileC opts prog)
    removeFileIfPossible
    ( \exe -> do
        maybePutStrLn externalSilent ""
        maybePutStrLn externalSilent "#### Running:"
        system exe >> return ()
    )

-- | Generate C code, use CC to compile it, and run the resulting executable
runCompiled :: (Interp instr CGen (Param2 exp pred), HFunctor instr) =>
    Program instr (Param2 exp pred) a -> IO ()
runCompiled = runCompiled' mempty

-- | Compile a program and make it available as an 'IO' function from 'String'
-- to 'String' (connected to @stdin@/@stdout@. respectively). Note that
-- compilation only happens once, even if the 'IO' function is used many times
-- in the body.
withCompiled' :: (Interp instr CGen (Param2 exp pred), HFunctor instr)
    => ExternalCompilerOpts
    -> Program instr (Param2 exp pred) a  -- ^ Program to compile
    -> ((String -> IO String) -> IO b)
         -- ^ Function that has access to the compiled executable as a function
    -> IO b
withCompiled' opts prog body = bracket
    (compileC opts prog)
    removeFileIfPossible
    (\exe -> body $ readProcess exe [])

-- | Compile a program and make it available as an 'IO' function from 'String'
-- to 'String' (connected to @stdin@/@stdout@. respectively). Note that
-- compilation only happens once, even if the 'IO' function is used many times
-- in the body.
withCompiled :: (Interp instr CGen (Param2 exp pred), HFunctor instr)
    => Program instr (Param2 exp pred) a  -- ^ Program to compile
    -> ((String -> IO String) -> IO b)
         -- ^ Function that has access to the compiled executable as a function
    -> IO b
withCompiled = withCompiled' defaultExtCompilerOpts {externalSilent = True}

-- | Like 'runCompiled'' but with explicit input/output connected to
-- @stdin@/@stdout@. Note that the program will be compiled every time the
-- function is applied to a string. In order to compile once and run many times,
-- use the function 'withCompiled''.
captureCompiled' :: (Interp instr CGen (Param2 exp pred), HFunctor instr)
    => ExternalCompilerOpts
    -> Program instr (Param2 exp pred) a  -- ^ Program to run
    -> String                             -- ^ Input to send to @stdin@
    -> IO String                          -- ^ Result from @stdout@
captureCompiled' opts prog inp = withCompiled' opts prog ($ inp)

-- | Like 'runCompiled' but with explicit input/output connected to
-- @stdin@/@stdout@. Note that the program will be compiled every time the
-- function is applied to a string. In order to compile once and run many times,
-- use the function 'withCompiled'.
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

