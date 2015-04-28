module Main where
import Prelude hiding (break)
import Language.Embedded.Imperative
import Language.Embedded.Concurrent
import Language.Embedded.Expr
import Language.Embedded.Backend.C ()
import Control.Applicative
import Control.Monad

-- For compilation
import Language.C.Monad
import Text.PrettyPrint.Mainland (Doc)

type L =
  ThreadCMD :+:
  ChanCMD Expr :+:
  ControlCMD Expr :+:
  FileCMD Expr

-- | Deadlocks due to channel becoming full.
deadlock :: Program L ()
deadlock = do
  c <- newChan 1
  t <- fork $ readChan c >>= printf "%d\n"
  writeChan c (1 :: Expr Int)
  writeChan c 2
  writeChan c 3
  printf "This never happens: %d\n" (4 :: Expr Int)

-- | Map a function over a file, then print the results. Mapping and printing
--   happen in separate threads.
mapFile :: (Expr Float -> Expr Float) -> FilePath -> Program L ()
mapFile f i = do
  c1 <- newCloseableChan 5
  c2 <- newCloseableChan 5
  fi <- fopen i ReadMode

  t1 <- fork $ do
    while (return true) $ do
      x <- readChan c1
      readOK <- lastChanReadOK c1
      iff readOK
        (void $ writeChan c2 (f x))
        (closeChan c2 >> break)

  t2 <- fork $ do
    while (lastChanReadOK c2) $ do
      readChan c2 >>= printf "%f\n"

  t3 <- fork $ do
    while (Not <$> feof fi) $ do
      fget fi >>= void . writeChan c1
    fclose fi
    closeChan c1
  waitThread t2

-- | Waiting for thread completion.
waiting :: Program L ()
waiting = do
  t <- fork $ printf "Forked thread printing %d\n" (0 :: Expr Int)
  waitThread t
  printf "Main thread printing %d\n" (1 :: Expr Int)

-- | A thread kills itself using its own thread ID.
suicide :: Program L ()
suicide = do
  tid <- forkWithId $ \tid -> do
    printf "This is printed. %d\n" (0 :: Expr Int)
    killThread tid
    printf "This is not. %d\n" (0 :: Expr Int)
  waitThread tid
  printf "The thread is dead, long live the thread! %d\n" (0 :: Expr Int)

-- | Compile a program. To compile the resulting C program:
--
--       gcc -Iinclude csrc/chan.c -lpthread YOURPROGRAM.c
--
compileProg :: (MapInstr instr, Interp instr CGen)
            => Program instr a
            -> Doc
compileProg = prettyCGen . liftSharedLocals . wrapMain . interpret
