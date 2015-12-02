{-# LANGUAGE CPP #-}

module Language.Embedded.Concurrent.CMD (
    TID, ThreadId (..),
    CID, ChanBound, Chan (..),
    ThreadCMD (..),
    ChanCMD (..),
    Closeable, Uncloseable
  ) where



#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Monad
import Control.Monad.Operational.Higher
import Data.IORef
import Data.Typeable
import Language.Embedded.Expression
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

