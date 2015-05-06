{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monads where



#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Monad.Exception
import Control.Monad.Identity
import Control.Monad.Reader
import qualified Control.Monad.State.Lazy as L
import Control.Monad.State.Strict
import Control.Monad.Writer

import Language.Embedded.Expression



newtype SupplyT m a = SupplyT { unSupplyT :: StateT VarId m a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadTrans)

type Supply = SupplyT Identity

class Monad m => MonadSupply m
  where
    -- | Create a fresh variable identifier
    fresh :: m VarId
    default fresh :: (m ~ t n, MonadTrans t, MonadSupply n) => m VarId
    fresh = lift fresh

instance Monad m => MonadSupply (SupplyT m)
  where
    fresh = do
        v <- SupplyT get
        SupplyT $ put (v+1)
        return v

instance MonadSupply m             => MonadSupply (ExceptionT m)
instance MonadSupply m             => MonadSupply (ReaderT  r m)
instance MonadSupply m             => MonadSupply (L.StateT s m)
instance MonadSupply m             => MonadSupply (StateT   s m)
instance (MonadSupply m, Monoid w) => MonadSupply (WriterT  w m)

instance MonadException m => MonadException (SupplyT m)
  where
    throw = lift . throw
    catch m h = SupplyT $ catch (unSupplyT m) (unSupplyT . h)

instance MonadReader r m => MonadReader r (SupplyT m)
  where
    ask     = lift ask
    local f = SupplyT . local f . unSupplyT

instance MonadState s m => MonadState s (SupplyT m)
  where
    get = lift get
    put = lift . put

instance MonadWriter w m => MonadWriter w (SupplyT m)
  where
    tell   = SupplyT . tell
    listen = SupplyT . listen . unSupplyT
    pass   = SupplyT . pass   . unSupplyT

runSupplyT :: Monad m => SupplyT m a -> m a
runSupplyT = flip evalStateT 0 . unSupplyT

runSupply :: Supply a -> a
runSupply = runIdentity . runSupplyT



-- | Program location
type Loc = Integer

-- | Tick monad
newtype TickT m a = TickT { unTickT :: StateT Loc m a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadTrans)

type Tick = TickT Identity

class Monad m => MonadTick m
  where
    tick :: m ()
    default tick :: (m ~ t n, MonadTrans t, MonadTick n) => m ()
    tick = lift tick
    loc  :: m Loc
    default loc :: (m ~ t n, MonadTrans t, MonadTick n) => m Loc
    loc = lift loc

instance Monad m => MonadTick (TickT m)
  where
    tick = do l <- loc; TickT $ put (l+1)
    loc  = TickT get

instance MonadTick m             => MonadTick (ReaderT  r m)
instance MonadTick m             => MonadTick (L.StateT s m)
instance MonadTick m             => MonadTick (StateT   s m)
instance (MonadTick m, Monoid w) => MonadTick (WriterT  w m)

instance MonadReader r m => MonadReader r (TickT m)
  where
    ask     = lift ask
    local f = TickT . local f . unTickT

instance MonadState s m => MonadState s (TickT m)
  where
    get = lift get
    put = lift . put

instance MonadWriter w m => MonadWriter w (TickT m)
  where
    tell   = TickT . tell
    listen = TickT . listen . unTickT
    pass   = TickT . pass   . unTickT

runTickT :: Monad m => TickT m a -> m a
runTickT = flip evalStateT 0 . unTickT

runTick :: Tick a -> a
runTick = runIdentity . runTickT

-- | Create a fresh string identifier with the given prefix
freshStr :: MonadSupply m => String -> m String
freshStr prefix = liftM ((prefix ++) . show) fresh

