{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

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
import Control.Monad.Operational.Higher
import Control.Monad.Reader
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
  show (TIDComp tid)   = tid

data Closeable
data Uncloseable

-- | A bounded channel.
data Chan t a
  = ChanEval (Bounded.BoundedChan a) (IORef Bool) (IORef Bool)
  | ChanComp CID

data ThreadCMD fs a where
  ForkWithId :: (ThreadId -> prog ()) -> ThreadCMD (Param3 prog exp pred) ThreadId
  Kill       :: ThreadId -> ThreadCMD (Param3 prog exp pred) ()
  Wait       :: ThreadId -> ThreadCMD (Param3 prog exp pred) ()

data ChanCMD fs a where
  NewChan   :: pred a => exp ChanBound -> ChanCMD (Param3 prog exp pred) (Chan t a)
  ReadChan  :: pred a => Chan t a -> ChanCMD (Param3 prog exp pred) (Val a)
  WriteChan :: pred a
            => Chan t a -> exp a -> ChanCMD (Param3 prog exp pred) (Val Bool)
  CloseChan :: Chan Closeable a -> ChanCMD (Param3 prog exp pred) ()
  ReadOK    :: Chan Closeable a -> ChanCMD (Param3 prog exp pred) (Val Bool)

instance HFunctor ThreadCMD where
  hfmap f (ForkWithId p) = ForkWithId $ f . p
  hfmap _ (Kill tid)     = Kill tid
  hfmap _ (Wait tid)     = Wait tid

instance HBifunctor ThreadCMD where
  hbimap f _ (ForkWithId p) = ForkWithId $ f . p
  hbimap _ _ (Kill tid)     = Kill tid
  hbimap _ _ (Wait tid)     = Wait tid

instance (ThreadCMD :<: instr) => Reexpressible ThreadCMD instr where
  reexpressInstrEnv reexp (ForkWithId p) = ReaderT $ \env ->
      singleInj $ ForkWithId (flip runReaderT env . p)
  reexpressInstrEnv reexp (Kill tid) = lift $ singleInj $ Kill tid
  reexpressInstrEnv reexp (Wait tid) = lift $ singleInj $ Wait tid

instance HFunctor ChanCMD where
  hfmap _ (NewChan sz)    = NewChan sz
  hfmap _ (ReadChan c)    = ReadChan c
  hfmap _ (WriteChan c x) = WriteChan c x
  hfmap _ (CloseChan c)   = CloseChan c
  hfmap _ (ReadOK c)      = ReadOK c

instance HBifunctor ChanCMD where
  hbimap _ f (NewChan sz)    = NewChan (f sz)
  hbimap _ _ (ReadChan c)    = ReadChan c
  hbimap _ f (WriteChan c x) = WriteChan c (f x)
  hbimap _ _ (CloseChan c)   = CloseChan c
  hbimap _ _ (ReadOK c)      = ReadOK c

instance (ChanCMD :<: instr) => Reexpressible ChanCMD instr where
  reexpressInstrEnv reexp (NewChan sz)    = lift . singleInj . NewChan =<< reexp sz
  reexpressInstrEnv reexp (ReadChan c)    = lift $ singleInj $ ReadChan c
  reexpressInstrEnv reexp (WriteChan c x) = lift . singleInj . WriteChan c =<< reexp x
  reexpressInstrEnv reexp (CloseChan c)   = lift $ singleInj $ CloseChan c
  reexpressInstrEnv reexp (ReadOK c)      = lift $ singleInj $ ReadOK c

runThreadCMD :: ThreadCMD (Param3 IO exp pred) a
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

runChanCMD :: ChanCMD (Param3 IO IO pred) a -> IO a
runChanCMD (NewChan sz) = do
  sz' <- sz
  ChanEval <$> Bounded.newBoundedChan (fromIntegral sz')
           <*> newIORef False
           <*> newIORef True
runChanCMD (ReadChan (ChanEval c closedref lastread)) = do
  closed <- readIORef closedref
  mval <- Bounded.tryReadChan c
  case mval of
    Just x -> do
        return $ ValEval x
    Nothing
      | closed -> do
        writeIORef lastread False
        return undefined
      | otherwise -> do
        ValEval <$> Bounded.readChan c
runChanCMD (WriteChan (ChanEval c closedref _) x) = do
  closed <- readIORef closedref
  x' <- x
  if closed
    then return (ValEval False)
    else Bounded.writeChan c x' >> return (ValEval True)
runChanCMD (CloseChan (ChanEval _ closedref _)) = do
  writeIORef closedref True
runChanCMD (ReadOK (ChanEval _ _ lastread)) = do
  ValEval <$> readIORef lastread

instance InterpBi ThreadCMD IO (Param1 pred) where
  interpBi = runThreadCMD
instance InterpBi ChanCMD IO (Param1 pred) where
  interpBi = runChanCMD

