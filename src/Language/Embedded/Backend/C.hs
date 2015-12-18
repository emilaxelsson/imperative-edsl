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

-- | Generate C code and use GCC to compile it
compileC :: (Interp instr CGen, HFunctor instr)
    => Bool             -- ^ Keep generated files?
    -> [String]         -- ^ GCC flags (e.g. @["-Ipath"]@)
    -> Program instr a  -- ^ Program to compile
    -> [String]         -- ^ GCC flags after C source (e.g. @["-lm","-lpthread"]@)
    -> IO FilePath      -- ^ Path to the generated executable
compileC keep flags prog postFlags = do
    tmp <- getTemporaryDirectory
    t   <- fmap (formatTime defaultTimeLocale format) getCurrentTime
    (exeFile,exeh) <- openTempFile tmp ("feldspar_" ++ t)
    hClose exeh
    let cFile = exeFile ++ ".c"
    writeFile cFile $ compile prog
    when keep $ putStrLn $ "Created temporary file: " ++ cFile
    let compileCMD = unwords
          $  ["gcc", "-std=c99"]
          ++ flags
          ++ [cFile, "-o", exeFile]
          ++ postFlags
    putStrLn compileCMD
    exit <- system compileCMD
    unless keep $ removeFileNiceIfPossible cFile
    case exit of
      ExitSuccess -> return exeFile
      err -> do removeFileNiceIfPossible exeFile
                error "compileC: failed to compile generated C code"
  where
    format = if keep then "%a-%H-%M-%S_" else ""

-- | Generate C code and use GCC to check that it compiles (no linking)
compileAndCheck :: (Interp instr CGen, HFunctor instr)
    => [String]         -- ^ GCC flags (e.g. @["-Ipath"]@)
    -> Program instr a  -- ^ Program to compile
    -> [String]         -- ^ GCC flags after C source (e.g. @["-lm","-lpthread"]@)
    -> IO ()
compileAndCheck flags prog postFlags = do
    exe <- compileC False ("-c":flags) prog postFlags
    removeFileNiceIfPossible exe

-- | Generate C code, use GCC to compile it, and run the resulting executable
runCompiled :: (Interp instr CGen, HFunctor instr)
    => [String]         -- ^ GCC flags (e.g. @["-Ipath"]@)
    -> Program instr a  -- ^ Program to compile
    -> [String]         -- ^ GCC flags after C source (e.g. @["-lm","-lpthread"]@)
    -> IO ()
runCompiled flags prog postFlags = do
    exe <- compileC False flags prog postFlags
    putStrLn ""
    putStrLn "#### Running:"
    system exe
    removeFileNiceIfPossible exe
    return ()

