{-# LANGUAGE CPP, GADTs, UndecidableInstances, QuasiQuotes #-}
-- | (Very) basic concurrency primitives.
module Language.Embedded.Concurrent (
    TID, ThreadId (..),
    CID, Chan (..),
    ThreadCMD (..),
    ChanCMD (..),
    fork, killThread,
    newChan, readChan, writeChan
  ) where
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Monad.Operational.Compositional
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

data ThreadId
  = TIDEval CC.ThreadId
  | TIDComp TID
    deriving (Typeable)

threadFun :: ThreadId -> String
threadFun tid = "thread_" ++ show tid

instance Show ThreadId where
  show (TIDEval tid) = show tid
  show (TIDComp tid) = show tid

-- | A bounded channel.
data Chan a
  = ChanEval (Bounded.BoundedChan a)
  | ChanComp CID

data ThreadCMD (prog :: * -> *) a where
  Fork :: prog () -> ThreadCMD prog ThreadId
  Kill :: ThreadId -> ThreadCMD prog ()

data ChanCMD p exp (prog :: * -> *) a where
  NewChan   :: p a => exp Int -> ChanCMD p exp prog (Chan a)
  ReadChan  :: p a => Chan a -> ChanCMD p exp prog (exp a)
  WriteChan :: p a => Chan a -> exp a -> ChanCMD p exp prog ()

instance MapInstr ThreadCMD where
  imap f (Fork p)   = Fork (f p)
  imap _ (Kill tid) = Kill tid

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
runThreadCMD (Fork p)           = TIDEval <$> CC.forkIO p
runThreadCMD (Kill (TIDEval t)) = CC.killThread t

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
killThread :: (ThreadCMD :<: instr)
           => ThreadId
           -> ProgramT instr m ()
killThread = singleton . inj . Kill

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
    addParam [cparam| void* |]
    body
    addStm [cstm| return NULL; |]
  addSystemInclude "pthread.h"
  addLocal [cdecl| typename pthread_t $id:tid; |]
  addStm [cstm| pthread_create(&$id:tid, NULL, $id:funName, NULL); |]
  return tid
compThreadCMD (Kill tid) = do
  addStm [cstm| pthread_cancel($id:tid); |]

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
