-- | Multi-element channels, for the Haskell interpretation of
--   'Language.Embedded.Concurrent'.
module Control.Chan where
import Control.Concurrent.STM

data ChanState = Open | Closed
  deriving Eq

newtype Chan a = Chan {unChan :: TVar (ChanGuts a)}

data ChanGuts a = ChanGuts
  { chanBuf        :: [a]
  , chanBufLen     :: Int
  , chanBound      :: Int
  , chanState      :: ChanState
  , chanLastReadOK :: Bool
  }

newChan :: Int -> IO (Chan a)
newChan len = fmap Chan . atomically . newTVar $ ChanGuts
  { chanBuf = []
  , chanBufLen = 0
  , chanBound = len
  , chanState = Open
  , chanLastReadOK = True
  }

readChan :: Chan a -> Int -> IO [a]
readChan (Chan chan) len = atomically $ do
  ch <- readTVar chan
  case chanState ch of
    Open -> do
      check (chanBufLen ch >= len)
      readAndUpdate ch True
    Closed
      | chanBufLen ch < len -> do
        return []
      | otherwise -> do
        readAndUpdate ch False
  where
    readAndUpdate ch success = do
      let (out, rest) = splitAt len (chanBuf ch)
      writeTVar chan $ ch
        { chanBuf = rest
        , chanBufLen = chanBufLen ch - len
        , chanLastReadOK = success
        }
      return out

writeChan :: Chan a -> [a] -> IO Bool
writeChan (Chan chan) xs = atomically $ do
  let len = length xs
  ch <- readTVar chan
  case chanState ch of
    Open -> do
      check (chanBound ch - chanBufLen ch >= len)
      writeTVar chan $ ch
        { chanBuf = chanBuf ch ++ xs
        , chanBufLen = chanBufLen ch + len
        }
      return True
    Closed -> do
      return False

closeChan :: Chan a -> IO ()
closeChan (Chan chan) = atomically $ do
  modifyTVar chan (\c -> c {chanState = Closed})

lastReadOK :: Chan a -> IO Bool
lastReadOK = fmap chanLastReadOK . atomically . readTVar . unChan
