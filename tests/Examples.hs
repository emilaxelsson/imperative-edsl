import Imperative ()
import Concurrent ()
import Language.Embedded.CExp
import Language.Embedded.Imperative
import Language.Embedded.Backend.C
import System.IO
import System.Process
import System.Directory
import System.Exit

main = do let c = compile $
                 do addInclude "<stdio.h>" :: Program (CallCMD CExp) ()
                    callProc "printf" [strArg "Hello World!\n"]
          (fp,h) <- openTempFile "" "temp.c"
          hPutStrLn h c
          hClose h
          system $ "gcc " ++ fp
          e <- system "./a.out"
          removeFile fp
          removeFile "a.out"
          exitWith e

