{-# LANGUAGE CPP, GADTs, UndecidableInstances, QuasiQuotes #-}
-- | Basic concurrency primitives.
module Language.Embedded.Concurrent (
    TID, ThreadId (..),
    CID, ChanBound, Chan (..),
    ThreadCMD (..),
    ChanCMD (..),
    Closeable, Uncloseable,
    fork, forkWithId, asyncKillThread, killThread, waitThread,
    newChan, newCloseableChan, readChan, writeChan,
    closeChan, lastChanReadOK,
  ) where
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Monad
import Control.Monad.Operational.Higher
import Data.IORef
import Data.Proxy
import Data.Typeable
import Language.Embedded.Expression
import Language.C.Quote.C
import Language.C.Monad
import qualified Language.C.Syntax as C
import qualified Control.Concurrent as CC
import qualified Control.Concurrent.BoundedChan as Bounded
import Data.Word (Word16)

-- | Maximum number of elements in some bounded channel.
type ChanBound = Word16

type TID = VarId
type CID = VarId

-- | A "flag" which may be waited upon. A flag starts of unset, and can be set
--   using 'setFlag'. Once set, the flag stays set forever.
data Flag a = Flag (IORef Bool) (CC.MVar a)

-- | Create a new, unset 'Flag'.
newFlag :: IO (Flag a)
newFlag = Flag <$> newIORef False <*> CC.newEmptyMVar

-- | Set a 'Flag'; guaranteed not to block.
--   If @setFlag@ is called on a flag which was already set, the value of said
--   flag is not updated.
--   @setFlag@ returns the status of the flag prior to the call: is the flag
--   was already set the return value is @True@, otherwise it is @False@.
setFlag :: Flag a -> a -> IO Bool
setFlag (Flag flag var) val = do
  set <- atomicModifyIORef flag $ \set -> (True, set)
  when (not set) $ CC.putMVar var val
  return set

-- | Wait until the given flag becomes set, then return its value. If the flag
--   is already set, return the value immediately.
waitFlag :: Flag a -> IO a
waitFlag (Flag _ var) = CC.withMVar var return

data ThreadId
  = TIDEval CC.ThreadId (Flag ())
  | TIDComp TID
    deriving (Typeable)

threadFun :: ThreadId -> String
threadFun tid = "thread_" ++ show tid

instance Show ThreadId where
  show (TIDEval tid _) = show tid
  show (TIDComp tid)   = show tid

data Closeable
data Uncloseable

-- | A bounded channel.
data Chan t a
  = ChanEval (Bounded.BoundedChan a) (IORef Bool) (IORef Bool)
  | ChanComp CID

data ThreadCMD (prog :: * -> *) a where
  ForkWithId :: (ThreadId -> prog ()) -> ThreadCMD prog ThreadId
  Kill       :: ThreadId -> ThreadCMD prog ()
  Wait       :: ThreadId -> ThreadCMD prog ()

data ChanCMD exp (prog :: * -> *) a where
  NewChan   :: VarPred exp a => exp ChanBound -> ChanCMD exp prog (Chan t a)
  ReadChan  :: VarPred exp a => Chan t a -> ChanCMD exp prog (exp a)
  WriteChan :: (VarPred exp a, VarPred exp Bool)
            => Chan t a -> exp a -> ChanCMD exp prog (exp Bool)
  CloseChan :: Chan Closeable a -> ChanCMD exp prog ()
  ReadOK    :: VarPred exp Bool
            => Chan Closeable a -> ChanCMD exp prog (exp Bool)

instance HFunctor ThreadCMD where
  hfmap f (ForkWithId p) = ForkWithId $ f . p
  hfmap _ (Kill tid)     = Kill tid
  hfmap _ (Wait tid)     = Wait tid

instance HFunctor (ChanCMD exp) where
  hfmap _ (NewChan sz)    = NewChan sz
  hfmap _ (ReadChan c)    = ReadChan c
  hfmap _ (WriteChan c x) = WriteChan c x
  hfmap _ (CloseChan c)   = CloseChan c
  hfmap _ (ReadOK c)      = ReadOK c

type instance IExp (ThreadCMD :+: i) = IExp i

type instance IExp (ChanCMD e)       = e
type instance IExp (ChanCMD e :+: i) = e

runThreadCMD :: ThreadCMD IO a
             -> IO a
runThreadCMD (ForkWithId p) = do
  f <- newFlag
  tidvar <- CC.newEmptyMVar
  cctid <- CC.forkIO . void $ CC.takeMVar tidvar >>= p >> setFlag f ()
  let tid = TIDEval cctid f
  CC.putMVar tidvar tid
  return tid
runThreadCMD (Kill (TIDEval t f)) = do
  setFlag f ()
  CC.killThread t
  return ()
runThreadCMD (Wait (TIDEval _ f)) = do
  waitFlag f

runChanCMD :: forall exp a. EvalExp exp
           => ChanCMD exp IO a -> IO a
runChanCMD (NewChan sz) =
  ChanEval <$> Bounded.newBoundedChan (fromIntegral $ evalExp sz)
           <*> newIORef False
           <*> newIORef True
runChanCMD (ReadChan (ChanEval c closedref lastread)) = do
  closed <- readIORef closedref
  mval <- Bounded.tryReadChan c
  case mval of
    Just x -> do
        return $ litExp x
    Nothing
      | closed -> do
        writeIORef lastread False
        return undefined
      | otherwise -> do
        litExp <$> Bounded.readChan c
runChanCMD (WriteChan (ChanEval c closedref _) x) = do
  closed <- readIORef closedref
  if closed
    then return (litExp False)
    else Bounded.writeChan c (evalExp x) >> return (litExp True)
runChanCMD (CloseChan (ChanEval _ closedref _)) = do
  writeIORef closedref True
runChanCMD (ReadOK (ChanEval _ _ lastread)) = do
  litExp <$> readIORef lastread

instance Interp ThreadCMD IO where
  interp = runThreadCMD
instance EvalExp exp => Interp (ChanCMD exp) IO where
  interp = runChanCMD

-- | Fork off a computation as a new thread.
fork :: (ThreadCMD :<: instr)
     => ProgramT instr m ()
     -> ProgramT instr m ThreadId
fork = forkWithId . const

-- | Fork off a computation as a new thread, with access to its own thread ID.
forkWithId :: (ThreadCMD :<: instr)
           => (ThreadId -> ProgramT instr m ())
           -> ProgramT instr m ThreadId
forkWithId = singleton . inj . ForkWithId

-- | Forcibly terminate a thread, then continue execution immediately.
asyncKillThread :: (ThreadCMD :<: instr) => ThreadId -> ProgramT instr m ()
asyncKillThread = singleton . inj . Kill

-- | Forcibly terminate a thread. Blocks until the thread is actually dead.
killThread :: (ThreadCMD :<: instr, Monad m) => ThreadId -> ProgramT instr m ()
killThread t = do
  singleton . inj $ Kill t
  waitThread t

-- | Wait for a thread to terminate.
waitThread :: (ThreadCMD :<: instr) => ThreadId -> ProgramT instr m ()
waitThread = singleton . inj . Wait

-- | Create a new channel. Writing a reference type to a channel will copy the
--   /reference/ into the queue, not its contents.
--
--   We'll likely want to change this, actually copying arrays and the like
--   into the queue instead of sharing them across threads.
newChan :: (VarPred (IExp instr) a, ChanCMD (IExp instr) :<: instr)
        => IExp instr ChanBound
        -> ProgramT instr m (Chan Uncloseable a)
newChan = singleE . NewChan

newCloseableChan :: (VarPred (IExp instr) a, ChanCMD (IExp instr) :<: instr)
        => IExp instr ChanBound
        -> ProgramT instr m (Chan Closeable a)
newCloseableChan = singleE . NewChan

-- | Read an element from a channel. If channel is empty, blocks until there
--   is an item available.
--   If 'closeChan' has been called on the channel *and* if the channel is
--   empty, @readChan@ returns an undefined value immediately.
readChan :: (VarPred (IExp instr) a, ChanCMD (IExp instr) :<: instr)
         => Chan t a
         -> ProgramT instr m (IExp instr a)
readChan = singleE . ReadChan

-- | Write a data element to a channel.
--   If 'closeChan' has been called on the channel, all calls to @writeChan@
--   become non-blocking no-ops and return @False@, otherwise returns @True@.
writeChan :: (VarPred (IExp instr) a,
              VarPred (IExp instr) Bool,
              ChanCMD (IExp instr) :<: instr)
        => Chan t a
        -> IExp instr a
        -> ProgramT instr m (IExp instr Bool)
writeChan c = singleE . WriteChan c

-- | When 'readChan' was last called on the given channel, did the read
--   succeed?
--   Always returns @True@ unless 'closeChan' has been called on the channel.
--   Always returns @True@ if the channel has never been read.
lastChanReadOK :: (VarPred (IExp instr) Bool, ChanCMD (IExp instr) :<: instr)
               => Chan Closeable a
               -> ProgramT instr m (IExp instr Bool)
lastChanReadOK = singleE . ReadOK

-- | Close a channel. All subsequent write operations will be no-ops.
--   After the channel is drained, all subsequent read operations will be
--   no-ops as well.
closeChan :: (ChanCMD (IExp instr) :<: instr)
          => Chan Closeable a
          -> ProgramT instr m ()
closeChan = singleE . CloseChan

instance ToIdent ThreadId where
  toIdent (TIDComp tid) = C.Id $ "t" ++ show tid

instance ToIdent (Chan t a) where
  toIdent (ChanComp c) = C.Id $ "chan" ++ show c

-- | Compile `ThreadCMD`.
--   TODO: sharing for threads with the same body
compThreadCMD :: ThreadCMD CGen a -> CGen a
compThreadCMD (ForkWithId body) = do
  tid <- TIDComp <$> freshId
  let funName = threadFun tid
  _ <- inFunctionTy [cty|void*|] funName $ do
    addParam [cparam| void* unused |]
    body tid
    addStm [cstm| return NULL; |]
  addSystemInclude "pthread.h"
  touchVar tid
  addLocal [cdecl| typename pthread_t $id:tid; |]
  addStm [cstm| pthread_create(&$id:tid, NULL, $id:funName, NULL); |]
  return tid
compThreadCMD (Kill tid) = do
  touchVar tid
  addStm [cstm| pthread_cancel($id:tid); |]
compThreadCMD (Wait tid) = do
  touchVar tid
  addStm [cstm| pthread_join($id:tid, NULL); |]

-- | Compile `ChanCMD`.
compChanCMD :: forall exp prog a. CompExp exp
            => ChanCMD exp prog a
            -> CGen a
compChanCMD cmd@(NewChan sz) = do
  addLocalInclude "chan.h"
  t <- compTypePP2 (Proxy :: Proxy exp) cmd
  sz' <- compExp sz
  c <- ChanComp <$> freshId
  addGlobal [cedecl| typename chan_t $id:c; |]
  addStm [cstm| $id:c = chan_new(sizeof($ty:t), $sz'); |]
  return c
compChanCMD (WriteChan c x) = do
  x' <- compExp x
  (v,name) <- freshVar
  (ok,okname) <- freshVar
  let _ = v `asTypeOf` x
  addStm [cstm| $id:name = $x'; |]
  addStm [cstm| $id:okname = chan_write($id:c, &$id:name); |]
  return ok
compChanCMD (ReadChan c) = do
  (var,name) <- freshVar
  addStm [cstm| chan_read($id:c, &$id:name); |]
  return var
compChanCMD (CloseChan c) = do
  addStm [cstm| chan_close($id:c); |]
compChanCMD (ReadOK c) = do
  (var,name) <- freshVar
  addStm [cstm| $id:name = chan_last_read_ok($id:c); |]
  return var

instance Interp ThreadCMD CGen where
  interp = compThreadCMD
instance CompExp exp => Interp (ChanCMD exp) CGen where
  interp = compChanCMD

