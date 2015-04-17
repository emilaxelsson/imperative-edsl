module Main where
import Language.Embedded.Imperative
import Language.Embedded.Concurrent
import Language.Embedded.Expr
import Language.Embedded.Backend.C ()
import Control.Applicative

-- For compilation
import Language.C.Monad
import Text.PrettyPrint.Mainland (Doc)

type Pred = VarPred Expr

type L =
  ThreadCMD :+:
  ChanCMD Pred Expr :+:
  ControlCMD Expr :+:
  ConsoleCMD Expr :+:
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
  c1 <- newChan 5
  c2 <- newChan 5
  t1 <- fork $ while (pure true) $ do
    readChan c1 >>= writeChan c2 . f
  t2 <- fork $ while (pure true) $ do
    readChan c2 >>= printf "%f\n"
  fi <- open i
  t3 <- fork $ do
    while (Not <$> feof fi) $ do
      fget fi >>= writeChan c1
    close fi
  waitThread t3

-- | Waiting for thread completion.
waiting :: Program L ()
waiting = do
  t <- fork $ printf "Forked thread printing %d\n" (0 :: Expr Int)
  waitThread t
  printf "Main thread printing %d\n" (1 :: Expr Int)

-- | Compile a program.
compileProg :: (MapInstr instr, Interp instr CGen)
            => Program instr a
            -> Doc
compileProg = prettyCGen . liftSharedLocals . wrapMain . interpret
