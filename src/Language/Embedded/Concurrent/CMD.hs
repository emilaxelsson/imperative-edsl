{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Embedded.Concurrent.CMD (
    TID, ThreadId (..),
    CID, Chan (..),
    ChanSize (..),
    timesSizeOf, timesSize, plusSize,
    ThreadCMD (..),
    ChanCMD (..),
    Closeable, Uncloseable
  ) where



#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
import Data.Typeable
#endif
import qualified Control.Chan as Chan
import qualified Control.Concurrent as CC
import Control.Monad.Operational.Higher
import Control.Monad.Reader
import Data.Dynamic
import Data.IORef
import Data.Ix (Ix)
import Data.Maybe (fromMaybe)

import Control.Monads
import Language.Embedded.Traversal
import Language.Embedded.Expression
import Language.Embedded.Imperative.CMD
import Language.Embedded.Imperative (getArr, setArr)



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
  = TIDRun CC.ThreadId (Flag ())
  | TIDComp TID
    deriving (Typeable)

instance Show ThreadId where
  show (TIDRun tid _) = show tid
  show (TIDComp tid)  = tid

data Closeable
data Uncloseable

-- | A bounded channel.
data Chan t a
  = ChanRun (Chan.Chan Dynamic)
  | ChanComp CID

-- | Channel size specification. For each possible element type, it shows how
--   many elements of them could be stored in the given channel at once.
data ChanSize exp pred i where
  OneSize   :: (pred a, Integral i) => proxy a -> exp i -> ChanSize exp pred i
  TimesSize :: Integral i => exp i -> ChanSize exp pred i -> ChanSize exp pred i
  PlusSize  :: Integral i => ChanSize exp pred i -> ChanSize exp pred i -> ChanSize exp pred i

mapSizeExp :: (exp i -> exp' i) -> ChanSize exp pred i -> ChanSize exp' pred i
mapSizeExp f (OneSize t sz) = OneSize t (f sz)
mapSizeExp f (TimesSize n sz) = TimesSize (f n) (mapSizeExp f sz)
mapSizeExp f (PlusSize a b) = PlusSize (mapSizeExp f a) (mapSizeExp f b)

mapSizeExpA :: (Functor m, Monad m)
            => (exp i -> m (exp' i))
            -> ChanSize exp pred i
            -> m (ChanSize exp' pred i)
mapSizeExpA f (OneSize t sz) = OneSize t <$> f sz
mapSizeExpA f (TimesSize n sz) = do
  n' <- f n
  sz' <- mapSizeExpA f sz
  return $ TimesSize n' sz'
mapSizeExpA f (PlusSize a b) = do
  a' <- mapSizeExpA f a
  b' <- mapSizeExpA f b
  return $ PlusSize a' b'

-- | Takes 'n' times the size of type refered by proxy.
timesSizeOf :: (pred a, Integral i) => exp i -> proxy a -> ChanSize exp pred i
timesSizeOf = flip OneSize

-- | Multiplies a channel size specification with a scalar.
timesSize :: Integral i => exp i -> ChanSize exp pred i -> ChanSize exp pred i
timesSize = TimesSize

-- | Adds two channel size specifications together.
plusSize :: Integral i => ChanSize exp pred i -> ChanSize exp pred i -> ChanSize exp pred i
plusSize = PlusSize

data ThreadCMD fs a where
  ForkWithId :: (ThreadId -> prog ()) -> ThreadCMD (Param3 prog exp pred) ThreadId
  Kill       :: ThreadId -> ThreadCMD (Param3 prog exp pred) ()
  Wait       :: ThreadId -> ThreadCMD (Param3 prog exp pred) ()
  Sleep      :: Integral i => exp i -> ThreadCMD (Param3 prog exp pred) ()

data ChanCMD fs a where
  NewChan   :: ChanSize exp pred i -> ChanCMD (Param3 prog exp pred) (Chan t c)
  CloseChan :: Chan Closeable c -> ChanCMD (Param3 prog exp pred) ()
  ReadOK    :: Chan Closeable c -> ChanCMD (Param3 prog exp pred) (Val Bool)

  ReadOne   :: (Typeable a, pred a)
            => Chan t c -> ChanCMD (Param3 prog exp pred) (Val a)
  WriteOne  :: (Typeable a, pred a)
            => Chan t c -> exp a -> ChanCMD (Param3 prog exp pred) (Val Bool)

  ReadChan  :: (Typeable a, pred a, Ix i, Integral i)
            => Chan t c -> exp i -> exp i
            -> Arr i a -> ChanCMD (Param3 prog exp pred) (Val Bool)
  WriteChan :: (Typeable a, pred a, Ix i, Integral i)
            => Chan t c -> exp i -> exp i
            -> Arr i a -> ChanCMD (Param3 prog exp pred) (Val Bool)

instance HFunctor ThreadCMD where
  hfmap f (ForkWithId p) = ForkWithId $ f . p
  hfmap _ (Kill tid)     = Kill tid
  hfmap _ (Wait tid)     = Wait tid
  hfmap _ (Sleep tid)    = Sleep tid

instance HBifunctor ThreadCMD where
  hbimap f _ (ForkWithId p) = ForkWithId $ f . p
  hbimap _ _ (Kill tid)     = Kill tid
  hbimap _ _ (Wait tid)     = Wait tid
  hbimap _ g (Sleep us)     = Sleep $ g us

instance DryInterp ThreadCMD where
  dryInterp (ForkWithId _) = liftM TIDComp $ freshStr "t"
  dryInterp (Kill _)       = return ()
  dryInterp (Wait _)       = return ()
  dryInterp (Sleep _)      = return ()

instance (ThreadCMD :<: instr) => Reexpressible ThreadCMD instr env where
  reexpressInstrEnv reexp (ForkWithId p) = ReaderT $ \env ->
      singleInj $ ForkWithId (flip runReaderT env . p)
  reexpressInstrEnv reexp (Kill tid) = lift $ singleInj $ Kill tid
  reexpressInstrEnv reexp (Wait tid) = lift $ singleInj $ Wait tid
  reexpressInstrEnv reexp (Sleep us) = (lift . singleInj . Sleep) =<< reexp us

instance HFunctor ChanCMD where
  hfmap _ (NewChan sz)        = NewChan sz
  hfmap _ (ReadOne c)         = ReadOne c
  hfmap _ (ReadChan c f t a)  = ReadChan c f t a
  hfmap _ (WriteOne c x)      = WriteOne c x
  hfmap _ (WriteChan c f t a) = WriteChan c f t a
  hfmap _ (CloseChan c)       = CloseChan c
  hfmap _ (ReadOK c)          = ReadOK c

instance HBifunctor ChanCMD where
  hbimap _ f (NewChan sz)         = NewChan (mapSizeExp f sz)
  hbimap _ _ (ReadOne c)          = ReadOne c
  hbimap _ f (ReadChan c n n' a)  = ReadChan c (f n) (f n') a
  hbimap _ f (WriteOne c x)       = WriteOne c (f x)
  hbimap _ f (WriteChan c n n' a) = WriteChan c (f n) (f n') a
  hbimap _ _ (CloseChan c    )    = CloseChan c
  hbimap _ _ (ReadOK c)           = ReadOK c

instance DryInterp ChanCMD where
  dryInterp (NewChan _)         = liftM ChanComp $ freshStr "chan"
  dryInterp (ReadOne _)         = liftM ValComp $ freshStr "v"
  dryInterp (ReadChan _ _ _ _)  = liftM ValComp $ freshStr "v"
  dryInterp (WriteOne _ _)      = liftM ValComp $ freshStr "v"
  dryInterp (WriteChan _ _ _ _) = liftM ValComp $ freshStr "v"
  dryInterp (CloseChan _)       = return ()
  dryInterp (ReadOK _)          = liftM ValComp $ freshStr "v"

instance (ChanCMD :<: instr) => Reexpressible ChanCMD instr env where
  reexpressInstrEnv reexp (NewChan sz) =
      lift . singleInj . NewChan =<< mapSizeExpA reexp sz
  reexpressInstrEnv reexp (ReadOne c) = lift $ singleInj $ ReadOne c
  reexpressInstrEnv reexp (ReadChan c f t a) = do
      rf <- reexp f
      rt <- reexp t
      lift $ singleInj $ ReadChan c rf rt a
  reexpressInstrEnv reexp (WriteOne c x)  = lift . singleInj . WriteOne c =<< reexp x
  reexpressInstrEnv reexp (WriteChan c f t a) = do
      rf <- reexp f
      rt <- reexp t
      lift $ singleInj $ WriteChan c rf rt a
  reexpressInstrEnv reexp (CloseChan c)   = lift $ singleInj $ CloseChan c
  reexpressInstrEnv reexp (ReadOK c)      = lift $ singleInj $ ReadOK c

runThreadCMD :: ThreadCMD (Param3 IO IO pred) a -> IO a
runThreadCMD (ForkWithId p) = do
  f <- newFlag
  tidvar <- CC.newEmptyMVar
  cctid <- CC.forkIO . void $ CC.takeMVar tidvar >>= p >> setFlag f ()
  let tid = TIDRun cctid f
  CC.putMVar tidvar tid
  return tid
runThreadCMD (Kill (TIDRun t f)) = do
  setFlag f ()
  CC.killThread t
  return ()
runThreadCMD (Wait (TIDRun _ f)) = do
  waitFlag f
runThreadCMD (Sleep us) = do
  us' <- us
  CC.threadDelay $ fromIntegral us'

runChanCMD :: forall pred a. ChanCMD (Param3 IO IO pred) a -> IO a
runChanCMD (NewChan sz) = do
  sz' <- evalChanSize sz
  ChanRun <$> Chan.newChan sz'
runChanCMD (ReadOne (ChanRun c)) =
  ValRun . convertDynamic . head <$> Chan.readChan c 1
runChanCMD (ReadChan (ChanRun c) off len arr) = do
  off' <- off
  len' <- len
  xs <- Chan.readChan c $ fromIntegral (len' - off')
  let xs' = map convertDynamic xs
  interpretBi id $ forM_ (zip [off' .. ] xs') $ \(i, x) -> do
    setArr (return i) (return x) arr :: Program ArrCMD (Param2 IO pred) ()
  ValRun <$> Chan.lastReadOK c
runChanCMD (WriteOne (ChanRun c) x) =
  ValRun <$> (Chan.writeChan c . return . toDyn =<< x)
runChanCMD (WriteChan (ChanRun c) off len (arr :: Arr ix el)) = do
  off' <- off
  len' <- len
  xs <- interpretBi id $ forM [off' .. off' + len' - 1] $ \i -> do
    getArr (return i) arr :: Program ArrCMD (Param2 IO pred) (IO el)
  ValRun <$> (Chan.writeChan c =<< map toDyn <$> sequence xs)
runChanCMD (CloseChan (ChanRun c)) = Chan.closeChan c
runChanCMD (ReadOK    (ChanRun c)) = ValRun <$> Chan.lastReadOK c

instance InterpBi ThreadCMD IO (Param1 pred) where
  interpBi = runThreadCMD
instance InterpBi ChanCMD IO (Param1 pred) where
  interpBi = runChanCMD

evalChanSize :: ChanSize IO pred i -> IO Int
evalChanSize (OneSize _ sz) = do
  sz' <- sz
  return $ fromIntegral sz'
evalChanSize (TimesSize n sz) = do
  n' <- n
  sz' <- evalChanSize sz
  return $ fromIntegral n' * sz'
evalChanSize (PlusSize a b) = do
  a' <- evalChanSize a
  b' <- evalChanSize b
  return $ a' + b'

convertDynamic :: Typeable a => Dynamic -> a
convertDynamic = fromMaybe (error "readChan: unknown element") . fromDynamic

instance FreeExp IO
  where
    type FreePred IO = Typeable
    constExp = return
    varExp   = error "varExp: unimplemented over IO"
