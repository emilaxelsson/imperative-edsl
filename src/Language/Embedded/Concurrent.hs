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

data ChanCMD p exp (prog :: * -> *) a where
  NewChan   :: p a => exp Int -> ChanCMD p exp prog (Chan a)
  ReadChan  :: p a => Chan a -> ChanCMD p exp prog (exp a)
  WriteChan :: p a => Chan a -> exp a -> ChanCMD p exp prog ()

instance MapInstr ThreadCMD where
  imap f (Fork p)   = Fork (f p)
  imap _ (Kill tid) = Kill tid
  imap _ (Wait tid) = Wait tid

instance MapInstr (ChanCMD p exp) where
  imap _ (NewChan sz)    = NewChan sz
  imap _ (ReadChan c)    = ReadChan c
  imap _ (WriteChan c x) = WriteChan c x

type instance IExp (ThreadCMD :+: i)  = IExp i
type instance IPred (ThreadCMD :+: i) = IPred i

type instance IExp (ChanCMD p e)        = e
type instance IExp (ChanCMD p e :+: i)  = e
type instance IPred (ChanCMD p e)       = p
type instance IPred (ChanCMD p e :+: i) = p

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

runChanCMD :: forall pred exp a. (VarPred exp ~ pred, EvalExp exp)
           => ChanCMD pred exp IO a -> IO a
runChanCMD (NewChan sz) =
  ChanEval <$> Bounded.newBoundedChan (evalExp sz)
runChanCMD (ReadChan (ChanEval c)) =
  litExp <$> Bounded.readChan c
runChanCMD (WriteChan (ChanEval c) x) =
  Bounded.writeChan c (evalExp x)

instance Interp ThreadCMD IO where
  interp = runThreadCMD
instance (VarPred exp ~ p, EvalExp exp) => Interp (ChanCMD p exp) IO where
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

-- | Create a new channel.
newChan :: (IPred instr a, ChanCMD (IPred instr) (IExp instr) :<: instr)
        => IExp instr Int
        -> ProgramT instr m (Chan a)
newChan = singlePE . NewChan

-- | Read an element from a channel. If channel is empty, blocks until there
--   is an item available.
readChan :: (IPred instr a, ChanCMD (IPred instr) (IExp instr) :<: instr)
         => Chan a
         -> ProgramT instr m (IExp instr a)
readChan = singlePE . ReadChan

-- | Write a data element to a channel.
writeChan :: (IPred instr a, ChanCMD (IPred instr) (IExp instr) :<: instr)
        => Chan a
        -> IExp instr a
        -> ProgramT instr m ()
writeChan c = singlePE . WriteChan c

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
            => ChanCMD (VarPred exp) exp prog a
            -> CGen a
compChanCMD cmd@(NewChan sz) = do
  addLocalInclude "queue.h"
  t <- compTypePP2 (Proxy :: Proxy exp) cmd
  sz' <- compExp sz
  c <- ChanComp <$> freshId
  addGlobal [cedecl| typename chan_t $id:c = chan_new($sz',sizeof($ty:t)); |]
  return c
compChanCMD (WriteChan c x) = do
  x' <- compExp x
  addStm [cstm| chan_write($id:c, $x'); |]
compChanCMD cmd@(ReadChan c) = do
  t <- compTypeP cmd
  ident <- freshId
  let name = 'v':show ident
      var = varExp ident
  e <- compExp var
  addLocal [cdecl| $ty:t $id:name; |]
  addStm [cstm| $e = chan_read($id:c); |]
  return var

instance Interp ThreadCMD CGen where
  interp = compThreadCMD
instance (VarPred exp ~ p, CompExp exp) => Interp (ChanCMD p exp) CGen where
  interp = compChanCMD
