{-# LANGUAGE CPP, GADTs, UndecidableInstances, QuasiQuotes #-}
-- | (Very) basic concurrency primitives.
module Language.Embedded.Concurrent (
    TID, ThreadId (..),
    CID, Chan (..),
    ThreadCMD (..),
    ChanCMD (..),
    fork, killThread, waitThread,
    newChan, readChan, writeChan
  ) where
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Monad
import Control.Monad.Operational.Compositional
import Data.IORef
import Data.Proxy
import Data.Typeable
import Language.Embedded.Imperative
import Language.Embedded.Backend.C (freshVar)
import Language.C.Quote.C
import Language.C.Monad
import qualified Language.C.Syntax as C
import qualified Control.Concurrent as CC
import qualified Control.Concurrent.BoundedChan as Bounded

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

-- | A bounded channel.
data Chan a
  = ChanEval (Bounded.BoundedChan a)
  | ChanComp CID

data ThreadCMD (prog :: * -> *) a where
  Fork :: prog () -> ThreadCMD prog ThreadId
  Kill :: ThreadId -> ThreadCMD prog ()
  Wait :: ThreadId -> ThreadCMD prog ()

data ChanCMD exp (prog :: * -> *) a where
  NewChan   :: VarPred exp a => exp Int -> ChanCMD exp prog (Chan a)
  ReadChan  :: VarPred exp a => Chan a -> ChanCMD exp prog (exp a)
  WriteChan :: VarPred exp a => Chan a -> exp a -> ChanCMD exp prog ()

instance MapInstr ThreadCMD where
  imap f (Fork p)   = Fork (f p)
  imap _ (Kill tid) = Kill tid
  imap _ (Wait tid) = Wait tid

instance MapInstr (ChanCMD exp) where
  imap _ (NewChan sz)    = NewChan sz
  imap _ (ReadChan c)    = ReadChan c
  imap _ (WriteChan c x) = WriteChan c x

type instance IExp (ThreadCMD :+: i) = IExp i

type instance IExp (ChanCMD e)       = e
type instance IExp (ChanCMD e :+: i) = e

runThreadCMD :: ThreadCMD IO a
             -> IO a
runThreadCMD (Fork p) = do
  f <- newFlag
  tid <- CC.forkIO . void $ p >> setFlag f ()
  return $ TIDEval tid f
runThreadCMD (Kill (TIDEval t f)) = do
  CC.killThread t
  setFlag f ()
  return ()
runThreadCMD (Wait (TIDEval _ f)) = do
  waitFlag f

runChanCMD :: forall exp a. EvalExp exp
           => ChanCMD exp IO a -> IO a
runChanCMD (NewChan sz) =
  ChanEval <$> Bounded.newBoundedChan (evalExp sz)
runChanCMD (ReadChan (ChanEval c)) =
  litExp <$> Bounded.readChan c
runChanCMD (WriteChan (ChanEval c) x) =
  Bounded.writeChan c (evalExp x)

instance Interp ThreadCMD IO where
  interp = runThreadCMD
instance EvalExp exp => Interp (ChanCMD exp) IO where
  interp = runChanCMD

-- | Fork off a computation as a new thread.
fork :: (ThreadCMD :<: instr)
     => ProgramT instr m ()
     -> ProgramT instr m ThreadId
fork = singleton . inj . Fork

-- | Forcibly terminate a thread.
killThread :: (ThreadCMD :<: instr) => ThreadId -> ProgramT instr m ()
killThread = singleton . inj . Kill

-- | Wait for a thread to terminate.
waitThread :: (ThreadCMD :<: instr) => ThreadId -> ProgramT instr m ()
waitThread = singleton . inj . Wait

-- | Create a new channel. Writing a reference type to a channel will copy the
--   *reference* into the queue, not its contents.
--
--   We'll likely want to change this, actually copying arrays and the like
--   into the queue instead of sharing them across threads.
newChan :: (VarPred (IExp instr) a, ChanCMD (IExp instr) :<: instr)
        => IExp instr Int
        -> ProgramT instr m (Chan a)
newChan = singleE . NewChan

-- | Read an element from a channel. If channel is empty, blocks until there
--   is an item available.
readChan :: (VarPred (IExp instr) a, ChanCMD (IExp instr) :<: instr)
         => Chan a
         -> ProgramT instr m (IExp instr a)
readChan = singleE . ReadChan

-- | Write a data element to a channel.
writeChan :: (VarPred (IExp instr) a, ChanCMD (IExp instr) :<: instr)
        => Chan a
        -> IExp instr a
        -> ProgramT instr m ()
writeChan c = singleE . WriteChan c

instance ToIdent ThreadId where
  toIdent (TIDComp tid) = C.Id $ "t" ++ show tid

instance ToIdent (Chan a) where
  toIdent (ChanComp c) = C.Id $ "chan" ++ show c

-- | Compile `ThreadCMD`.
--   TODO: sharing for threads with the same body; sharing closed-over vars.
compThreadCMD:: ThreadCMD CGen a -> CGen a
compThreadCMD (Fork body) = do
  tid <- TIDComp <$> freshId
  let funName = threadFun tid
  _ <- inFunctionTy [cty|void*|] funName $ do
    addParam [cparam| void* unused |]
    body
    addStm [cstm| return NULL; |]
  addSystemInclude "pthread.h"
  addLocal [cdecl| typename pthread_t $id:tid; |]
  addStm [cstm| pthread_create(&$id:tid, NULL, $id:funName, NULL); |]
  return tid
compThreadCMD (Kill tid) = do
  addStm [cstm| pthread_cancel($id:tid); |]
compThreadCMD (Wait tid) = do
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
  let _ = v `asTypeOf` x
  addStm [cstm| $id:name = $x'; |]
  addStm [cstm| chan_write($id:c, &$id:name); |]
compChanCMD (ReadChan c) = do
  (var,name) <- freshVar
  addStm [cstm| chan_read($id:c, &$id:name); |]
  return var

instance Interp ThreadCMD CGen where
  interp = compThreadCMD
instance CompExp exp => Interp (ChanCMD exp) CGen where
  interp = compChanCMD
