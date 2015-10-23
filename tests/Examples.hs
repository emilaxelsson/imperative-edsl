import Imperative ()
import Concurrent ()
import Language.Embedded.Expr
import Language.Embedded.Imperative
import System.IO
import System.Process
import System.Directory
import System.Exit

main = do let c = compile $
                 do addInclude "<stdio.h>" :: Program (CallCMD Expr) ()
                    callProc "printf" [StrArg "Hello World!\n"]
          (fp,h) <- openTempFile "" "temp.c"
          hPutStrLn h c
          hClose h
          system $ "gcc " ++ fp
          e <- system "./a.out"
          removeFile fp
          removeFile "a.out"
          exitWith e

