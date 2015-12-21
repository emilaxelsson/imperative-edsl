{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

-- | C code generation for 'Program'

module Language.Embedded.Backend.C where



#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Exception
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import System.Directory (getTemporaryDirectory, removeFile)
import System.Exit (ExitCode (..))
import System.IO
import System.Process (system)

import Data.Loc (noLoc)
import qualified Language.C.Syntax as C

import Control.Monad.Operational.Higher
import Language.C.Monad

import Text.PrettyPrint.Mainland (pretty)



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



--------------------------------------------------------------------------------
-- * Code generation user interface
--------------------------------------------------------------------------------

-- | Compile a program to C code represented as a string
--
-- For programs that make use of the primitives in
-- "Language.Embedded.Concurrent", the resulting C code can be compiled as
-- follows:
--
-- > gcc -Iinclude csrc/chan.c -lpthread YOURPROGRAM.c
compile :: (Interp instr CGen, HFunctor instr) => Program instr a -> String
compile = pretty 80 . prettyCGen . liftSharedLocals . wrapMain . interpret

-- | Compile a program to C code and print it on the screen
--
-- For programs that make use of the primitives in
-- "Language.Embedded.Concurrent", the resulting C code can be compiled as
-- follows:
--
-- > gcc -Iinclude csrc/chan.c -lpthread YOURPROGRAM.c
icompile :: (Interp instr CGen, HFunctor instr) => Program instr a -> IO ()
icompile = putStrLn . compile

removeFileNiceIfPossible :: FilePath -> IO ()
removeFileNiceIfPossible file =
    catch (removeFile file) (\(_ :: SomeException) -> return ())

data ExternalCompilerOpts = ExternalCompilerOpts
      { externalKeepFiles  :: Bool      -- ^ Keep generated files?
      , externalFlagsPre   :: [String]  -- ^ External compiler flags (e.g. @["-Ipath"]@)
      , externalFlagsPost  :: [String]  -- ^ External compiler flags after C source (e.g. @["-lm","-lpthread"]@)
      }

defaultExtCompilerOpts :: ExternalCompilerOpts
defaultExtCompilerOpts = ExternalCompilerOpts
    { externalKeepFiles = False
    , externalFlagsPre  = []
    , externalFlagsPost = []
    }

instance Monoid ExternalCompilerOpts
  where
    mempty = defaultExtCompilerOpts
    mappend (ExternalCompilerOpts keep1 pre1 post1) (ExternalCompilerOpts keep2 pre2 post2) =
        ExternalCompilerOpts keep2 (pre1 ++ pre2) (post1 ++ post2)

-- | Generate C code and use GCC to compile it
compileC :: (Interp instr CGen, HFunctor instr)
    => ExternalCompilerOpts
    -> Program instr a  -- ^ Program to compile
    -> IO FilePath      -- ^ Path to the generated executable
compileC (ExternalCompilerOpts {..}) prog = do
    tmp <- getTemporaryDirectory
    t   <- fmap (formatTime defaultTimeLocale format) getCurrentTime
    (exeFile,exeh) <- openTempFile tmp ("feldspar_" ++ t)
    hClose exeh
    let cFile = exeFile ++ ".c"
    writeFile cFile $ compile prog
    when externalKeepFiles $ putStrLn $ "Created temporary file: " ++ cFile
    let compileCMD = unwords
          $  ["gcc", "-std=c99"]
          ++ externalFlagsPre
          ++ [cFile, "-o", exeFile]
          ++ externalFlagsPost
    putStrLn compileCMD
    exit <- system compileCMD
    unless externalKeepFiles $ removeFileNiceIfPossible cFile
    case exit of
      ExitSuccess -> return exeFile
      err -> do removeFileNiceIfPossible exeFile
                error "compileC: failed to compile generated C code"
  where
    format = if externalKeepFiles then "%a-%H-%M-%S_" else ""

-- | Generate C code and use GCC to check that it compiles (no linking)
compileAndCheck' :: (Interp instr CGen, HFunctor instr) =>
    ExternalCompilerOpts -> Program instr a -> IO ()
compileAndCheck' opts prog = do
    exe <- compileC opts prog
    removeFileNiceIfPossible exe

-- | Generate C code and use GCC to check that it compiles (no linking)
compileAndCheck :: (Interp instr CGen, HFunctor instr) =>
    Program instr a -> IO ()
compileAndCheck = compileAndCheck' mempty

-- | Generate C code, use GCC to compile it, and run the resulting executable
runCompiled' :: (Interp instr CGen, HFunctor instr) =>
    ExternalCompilerOpts -> Program instr a -> IO ()
runCompiled' opts prog = do
    exe <- compileC opts prog
    putStrLn ""
    putStrLn "#### Running:"
    system exe
    removeFileNiceIfPossible exe
    return ()

-- | Generate C code, use GCC to compile it, and run the resulting executable
runCompiled :: (Interp instr CGen, HFunctor instr) => Program instr a -> IO ()
runCompiled = runCompiled' mempty

