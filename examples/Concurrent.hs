{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Concurrent where

import Prelude hiding (break)

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

import Language.Embedded.Imperative
import Language.Embedded.Concurrent
import Language.Embedded.Backend.C
import Language.Embedded.CExp

type CMD =
  ThreadCMD :+:
  ChanCMD :+:
  ControlCMD :+:
  FileCMD :+:
  ArrCMD

type Prog = Program CMD (Param2 CExp CType)

-- | Deadlocks due to channel becoming full.
deadlock :: Prog ()
deadlock = do
  c <- newChan (1 :: CExp Word32)
  t <- fork $ readChan c >>= printf "%d\n"
  writeChan c (1 :: CExp Int32)
  writeChan c 2
  writeChan c 3
  printf "This never happens: %d\n" (4 :: CExp Int32)

-- | Map a function over a file, then print the results. Mapping and printing
--   happen in separate threads.
mapFile :: (CExp Float -> CExp Float) -> FilePath -> Prog ()
mapFile f i = do
  c1 <- newCloseableChan (5 :: CExp Word32)
  c2 <- newCloseableChan (5 :: CExp Word32)
  fi <- fopen i ReadMode

  t1 <- fork $ do
    while (return true) $ do
      x <- readChan c1
      readOK <- lastChanReadOK c1
      iff readOK
        (void $ writeChan c2 (f x))
        (closeChan c2 >> break)

  t2 <- fork $ do
    while (return true) $ do
      x <- readChan c2
      readOK <- lastChanReadOK c2
      iff readOK
        (printf "%f\n" x)
        (break)

  t3 <- fork $ do
    while (not_ <$> feof fi) $ do
      x <- fget fi
      eof <- feof fi
      iff eof
        (break)
        (void $ writeChan c1 x)
    fclose fi
    closeChan c1
  waitThread t2

-- | Waiting for thread completion.
waiting :: Prog ()
waiting = do
  t <- fork $ printf "Forked thread printing %d\n" (0 :: CExp Int32)
  waitThread t
  printf "Main thread printing %d\n" (1 :: CExp Int32)

-- | A thread kills itself using its own thread ID.
suicide :: Prog ()
suicide = do
  tid <- forkWithId $ \tid -> do
    printf "This is printed. %d\n" (0 :: CExp Int32)
    killThread tid
    printf "This is not. %d\n" (0 :: CExp Int32)
  waitThread tid
  printf "The thread is dead, long live the thread! %d\n" (0 :: CExp Int32)


-- | Primitive channel operations.
chanOps :: Prog ()
chanOps = do
  c <- newCloseableChan (2 :: CExp Word32)
  writeChan c (1337 :: CExp Int32)
  writeChan c 42
  a <- readChan c
  b <- readChan c
  printf "%d %d\n" a b

  sent :: Arr Int8 Int32 <- constArr [ 12, 34 ]
  writeChanBuf c (0 :: CExp Int8) 2 sent
  received <- newArr (2 :: CExp Int8)
  readChanBuf c (0 :: CExp Int8) 2 received
  a <- getArr received 0
  b <- getArr received 1
  printf "%d %d\n" a b

  writeChan' c (67 :: CExp Word8)
  r :: CExp Word8 <- readChan' c
  printf "%d\n" r
  closeChan c



----------------------------------------

testAll = do
    tag "waiting" >> compareCompiled' opts waiting (runIO waiting) ""
    tag "suicide" >> compareCompiled' opts suicide (runIO suicide) ""
    tag "chanOps" >> compareCompiled' opts chanOps (runIO chanOps) ""
  where
    tag str = putStrLn $ "---------------- examples/Concurrent.hs/" ++ str ++ "\n"
    opts = def
         { externalFlagsPre  = ["-Iinclude", "csrc/chan.c"]
         , externalFlagsPost = ["-lpthread"]
         }
