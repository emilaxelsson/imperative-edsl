-- | Basic concurrency primitives.
module Language.Embedded.Concurrent (
    ThreadId (..),
    ChanBound, Chan (..),
    ThreadCMD,
    ChanCMD,
    Closeable, Uncloseable,
    fork, forkWithId, asyncKillThread, killThread, waitThread,
    newChan, newCloseableChan, readChan, writeChan,
    closeChan, lastChanReadOK,
  ) where

import Control.Monad.Operational.Higher
import Language.Embedded.Expression
import Language.Embedded.Concurrent.CMD
import Language.Embedded.Concurrent.Backend.C ()

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

