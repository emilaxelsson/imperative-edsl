-- | Basic concurrency primitives.
--
-- To compile the C code resulting from 'Language.Embedded.Backend.C.compile'
-- for programs with concurrency primitives, use something like
--
-- > gcc -std=c99 -Iinclude csrc/chan.c -lpthread YOURPROGRAM.c
module Language.Embedded.Concurrent
  ( ThreadId (..)
  , Chan (..), Transferable (..), BulkTransferable(..)
  , ThreadCMD
  , ChanCMD
  , Closeable, Uncloseable
  , fork, forkWithId, asyncKillThread, killThread, waitThread
  , readChan', writeChan'
  , readChanBuf', writeChanBuf'
  , closeChan, lastChanReadOK
  ) where

import Control.Monad.Operational.Higher
import Data.Ix
import Data.Typeable
import Data.Word

import Language.Embedded.Backend.C (CType)
import Language.Embedded.CExp (CExp)
import Language.Embedded.Concurrent.Backend.C ()
import Language.Embedded.Concurrent.CMD
import Language.Embedded.Expression
import Language.Embedded.Imperative.CMD (Arr)



-- | Fork off a computation as a new thread.
fork :: (ThreadCMD :<: instr)
     => ProgramT instr (Param2 exp pred) m ()
     -> ProgramT instr (Param2 exp pred) m ThreadId
fork = forkWithId . const

-- | Fork off a computation as a new thread, with access to its own thread ID.
forkWithId :: (ThreadCMD :<: instr)
           => (ThreadId -> ProgramT instr (Param2 exp pred) m ())
           -> ProgramT instr (Param2 exp pred) m ThreadId
forkWithId = singleton . inj . ForkWithId

-- | Forcibly terminate a thread, then continue execution immediately.
asyncKillThread :: (ThreadCMD :<: instr)
                => ThreadId -> ProgramT instr (Param2 exp pred) m ()
asyncKillThread = singleton . inj . Kill

-- | Forcibly terminate a thread. Blocks until the thread is actually dead.
killThread :: (ThreadCMD :<: instr, Monad m)
           => ThreadId -> ProgramT instr (Param2 exp pred) m ()
killThread t = do
  singleton . inj $ Kill t
  waitThread t

-- | Wait for a thread to terminate.
waitThread :: (ThreadCMD :<: instr)
           => ThreadId -> ProgramT instr (Param2 exp pred) m ()
waitThread = singleton . inj . Wait


--------------------------------------------------------------------------------
-- Channel interface
--------------------------------------------------------------------------------

class Transferable exp pred a
  where
    -- | Size specification of a channel. In most of the cases, it is a natural
    --   number representing how many elements could be stored at the same time
    --   in the channel.
    type SizeSpec a :: *

    -- | Maps a size specification to an internal channel size representation,
    --   that is a map from primitive types to quantities. The byte size of the
    --   channel will be calculated as the sum of multiplying the byte size of
    --   each type with its quantity.
    calcChanSize :: proxy a -> SizeSpec a -> ChanSize exp pred

    -- | Create a new channel. Writing a reference type to a channel will copy
    --   contents into the channel, so modifying it post-write is completely
    --   safe.
    newChan :: (ChanCMD :<: instr)
            => SizeSpec a
            -> ProgramT instr (Param2 exp pred) m (Chan Uncloseable a)
    newChan = singleInj . NewChan . calcChanSize (Proxy :: Proxy a)

    newCloseableChan :: (ChanCMD :<: instr)
                     => SizeSpec a
                     -> ProgramT instr (Param2 exp pred) m (Chan Closeable a)
    newCloseableChan = singleInj . NewChan . calcChanSize (Proxy :: Proxy a)

    -- | Read an element from a channel. If channel is empty, blocks until there
    --   is an item available.
    --   If 'closeChan' has been called on the channel *and* if the channel is
    --   empty, @readChan@ returns an undefined value immediately.
    readChan :: ( ChanCMD :<: instr, Monad m )
             => Chan t a
             -> ProgramT instr (Param2 exp pred) m a

    -- | Write a data element to a channel.
    --   If 'closeChan' has been called on the channel, all calls to @writeChan@
    --   become non-blocking no-ops and return @False@, otherwise returns @True@.
    --   If the channel is full, this function blocks until there's space in the
    --   queue.
    writeChan :: ( ChanCMD :<: instr, Monad m )
              => Chan t a
              -> a
              -> ProgramT instr (Param2 exp pred) m (exp Bool)

instance CType a => Transferable CExp CType (CExp a)
  where
    type SizeSpec (CExp a) = CExp Word32
    calcChanSize _ sz = ChanSize [(ChanElemType (Proxy :: Proxy a), sz)]
    readChan  = readChan'
    writeChan = writeChan'

class BulkTransferable exp pred i a
  where
    type ChanType a :: *
    -- | Read an arbitrary number of elements from a channel into an array.
    --   The semantics are the same as for 'readChan', where "channel is empty"
    --   is defined as "channel contains less data than requested".
    --   Returns @False@ without reading any data if the channel is closed.
    readChanBuf :: ( Ix i, Integral i
                   , FreeExp exp, FreePred exp Bool
                   , ChanCMD :<: instr, Monad m )
                => Chan t (ChanType a)
                -> exp i -- ^ Offset in array to start writing
                -> exp i -- ^ Elements to read
                -> a
                -> ProgramT instr (Param2 exp pred) m (exp Bool)

    -- | Write an arbitrary number of elements from an array into an channel.
    --   The semantics are the same as for 'writeChan', where "channel is full"
    --   is defined as "channel has insufficient free space to store all written
    --   data".
    writeChanBuf :: ( Ix i, Integral i
                    , FreeExp exp, FreePred exp Bool
                    , ChanCMD :<: instr, Monad m )
                 => Chan t (ChanType a)
                 -> exp i -- ^ Offset in array to start reading
                 -> exp i -- ^ Elements to write
                 -> a
                 -> ProgramT instr (Param2 exp pred) m (exp Bool)

instance CType a => BulkTransferable CExp CType i (Arr i a)
  where
    type ChanType (Arr i a) = CExp a
    readChanBuf  = readChanBuf'
    writeChanBuf = writeChanBuf'


-- | When 'readChan' was last called on the given channel, did the read
--   succeed?
--   Always returns @True@ unless 'closeChan' has been called on the channel.
--   Always returns @True@ if the channel has never been read.
lastChanReadOK :: (FreeExp exp, FreePred exp Bool, ChanCMD :<: instr, Monad m)
               => Chan Closeable c
               -> ProgramT instr (Param2 exp pred) m (exp Bool)
lastChanReadOK = fmap valToExp . singleInj . ReadOK

-- | Close a channel. All subsequent write operations will be no-ops.
--   After the channel is drained, all subsequent read operations will be
--   no-ops as well.
closeChan :: (ChanCMD :<: instr)
          => Chan Closeable c
          -> ProgramT instr (Param2 exp pred) m ()
closeChan = singleInj . CloseChan


--------------------------------------------------------------------------------
-- Channel primitives
--------------------------------------------------------------------------------

readChan' :: ( Typeable a, pred a
             , FreeExp exp, FreePred exp a
             , ChanCMD :<: instr, Monad m )
          => Chan t c
          -> ProgramT instr (Param2 exp pred) m (exp a)
readChan' = fmap valToExp . singleInj . ReadOne

readChanBuf' :: ( Typeable a, pred a
                , Ix i, Integral i
                , FreeExp exp, FreePred exp Bool
                , ChanCMD :<: instr, Monad m )
             => Chan t c
             -> exp i -- ^ Offset in array to start writing
             -> exp i -- ^ Elements to read
             -> Arr i a
             -> ProgramT instr (Param2 exp pred) m (exp Bool)
readChanBuf' ch off sz arr = fmap valToExp . singleInj $ ReadChan ch off sz arr

writeChan' :: ( Typeable a, pred a
              , FreeExp exp, FreePred exp Bool
              , ChanCMD :<: instr, Monad m )
           => Chan t c
           -> exp a
           -> ProgramT instr (Param2 exp pred) m (exp Bool)
writeChan' c = fmap valToExp . singleInj . WriteOne c

writeChanBuf' :: ( Typeable a, pred a
                 , Ix i, Integral i
                 , FreeExp exp, FreePred exp Bool
                 , ChanCMD :<: instr, Monad m )
              => Chan t c
              -> exp i -- ^ Offset in array to start reading
              -> exp i -- ^ Elements to write
              -> Arr i a
              -> ProgramT instr (Param2 exp pred) m (exp Bool)
writeChanBuf' ch off sz arr = fmap valToExp . singleInj $ WriteChan ch off sz arr
