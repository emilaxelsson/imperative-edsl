{-# LANGUAGE CPP, GADTs, UndecidableInstances #-}
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
import Data.Typeable
import Language.Embedded.Imperative
import qualified Control.Concurrent as CC
import qualified Control.Concurrent.BoundedChan as Bounded

type TID = Int
type CID = Int

data ThreadId
  = TIDEval CC.ThreadId
  | TIDComp TID
    deriving (Typeable, Show)

-- | A bounded channel.
data Chan a
  = ChanEval (Bounded.BoundedChan a)
  | ChanComp CID

data ThreadCMD exp (prog :: * -> *) a where
  Fork :: prog () -> ThreadCMD exp prog (exp ThreadId)
  Kill :: exp ThreadId -> ThreadCMD exp prog ()

data ChanCMD p exp (prog :: * -> *) a where
  NewChan   :: exp Int -> ChanCMD p exp prog (Chan a)
  ReadChan  :: p a => Chan a -> ChanCMD p exp prog (exp a)
  WriteChan :: p a => Chan a -> exp a -> ChanCMD p exp prog ()

instance MapInstr (ThreadCMD exp) where
  imap f (Fork p)   = Fork (f p)
  imap _ (Kill tid) = Kill tid

instance MapInstr (ChanCMD p exp) where
  imap _ (NewChan sz)    = NewChan sz
  imap _ (ReadChan c)    = ReadChan c
  imap _ (WriteChan c x) = WriteChan c x

type instance IExp (ThreadCMD e)       = e
type instance IExp (ThreadCMD e :+: i) = e

type instance IExp (ChanCMD p e)       = e
type instance IExp (ChanCMD p e :+: i) = e

runThreadCMD :: (VarPred exp ThreadId, EvalExp exp)
             => ThreadCMD exp IO a
             -> IO a
runThreadCMD (Fork p) =
  litExp . TIDEval <$> CC.forkIO p
runThreadCMD (Kill t) =
  case evalExp t of
    TIDEval t' -> CC.killThread t'
    _          -> error "Kill: impossible!"

runChanCMD :: forall pred exp a. (VarPred exp ~ pred, EvalExp exp)
           => ChanCMD pred exp IO a -> IO a
runChanCMD (NewChan sz) =
  ChanEval <$> Bounded.newBoundedChan (evalExp sz)
runChanCMD (ReadChan (ChanEval c)) =
  litExp <$> Bounded.readChan c
runChanCMD (WriteChan (ChanEval c) x) =
  Bounded.writeChan c (evalExp x)

instance (VarPred exp ThreadId, EvalExp exp) => Interp (ThreadCMD exp) IO where
  interp = runThreadCMD
instance (VarPred exp ~ p, EvalExp exp) => Interp (ChanCMD p exp) IO where
  interp = runChanCMD

-- | Fork off a computation as a new thread.
fork :: (ThreadCMD (IExp instr) :<: instr)
     => ProgramT instr m ()
     -> ProgramT instr m (IExp instr ThreadId)
fork = singleE . Fork

-- | Forcibly terminate a thread.
killThread :: (ThreadCMD (IExp instr) :<: instr)
           => IExp instr ThreadId
           -> ProgramT instr m ()
killThread = singleE . Kill

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
