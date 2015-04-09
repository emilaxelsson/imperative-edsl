{-# LANGUAGE CPP #-}

-- | An alternative to the Operational package \[1\] in which instructions are parameterized on the
-- program monad that they are part of. This makes it possible to define instruction sets
-- compositionally using ':+:'. In the normal Operational, this can only be done for simple
-- instructions, but here it can be done even for \"control instructions\" -- instructions that take
-- program as arguments.
--
-- For example, an \"if\" instruction can be defined as follows:
--
-- > data If p a where
-- >   If :: Bool -> p a -> p a -> If p a
--
-- This module also uses an implementation of Data Types à la Carte to make it easier to work with
-- compound instruction sets.
--
-- For more information about how to use this module, see the Operational package \[1\].
--
-- \[1\] <http://hackage.haskell.org/package/operational>

module Control.Monad.Operational.Compositional
    ( module Data.ALaCarte
    , ProgramT
    , Program
    , singleton
    , singleInj
    , MapInstr (..)
    , liftProgram
    , interpretWithMonadT
    , interpretWithMonad
    , Interp (..)
    , interpretT
    , interpret
    ) where



import Control.Applicative (Applicative (..))
import Control.Monad.Identity
import Control.Monad.Trans
import Data.Typeable

import Data.ALaCarte



-- | Representation of programs parameterized by the primitive instructions
data ProgramT instr m a
  where
    Lift  :: m a -> ProgramT instr m a
    Bind  :: ProgramT instr m a -> (a -> ProgramT instr m b) -> ProgramT instr m b
    Instr :: instr (ProgramT instr m) a -> ProgramT instr m a
#if  __GLASGOW_HASKELL__>=708
  deriving Typeable
#endif

-- | Representation of programs parameterized by its primitive instructions
type Program instr = ProgramT instr Identity

instance Monad m => Functor (ProgramT instr m)
  where
    fmap = liftM

instance Monad m => Applicative (ProgramT instr m)
  where
    pure  = return
    (<*>) = ap

instance Monad m => Monad (ProgramT instr m)
  where
    return = Lift . return
    (>>=)  = Bind

instance MonadTrans (ProgramT instr)
  where
    lift = Lift

-- | Make a program from a single primitive instruction
singleton :: instr (ProgramT instr m) a -> ProgramT instr m a
singleton = Instr

-- | Make a program from a single primitive instruction
singleInj :: (i :<: instr) => i (ProgramT instr m) a -> ProgramT instr m a
singleInj = Instr . inj

-- | Class for mapping over the sub-programs of instructions
class MapInstr instr
  where
    -- | Map over the sub-programs of instructions
    imap :: (forall b . m b -> n b) -> instr m a -> instr n a

instance (MapInstr i1, MapInstr i2) => MapInstr (i1 :+: i2)
  where
    imap f (Inl i) = Inl $ imap f i
    imap f (Inr i) = Inr $ imap f i

-- | Lift a simple program to a program over a monad @m@
liftProgram :: forall instr m a . (MapInstr instr, Monad m) => Program instr a -> ProgramT instr m a
liftProgram = go
  where
    go :: Program instr b -> ProgramT instr m b
    go (Lift a)   = Lift $ return $ runIdentity a
    go (Bind p k) = Bind (go p) (go . k)
    go (Instr i)  = Instr $ imap go i

-- | Interpret a program in a monad
interpretWithMonadT :: forall instr m n a . (MapInstr instr, Monad m)
    => (forall b . instr m b -> m b)
    -> (forall b . n b -> m b)
    -> ProgramT instr n a -> m a
interpretWithMonadT runi runn = go
  where
    go :: ProgramT instr n b -> m b
    go (Lift a)   = runn a
    go (Bind p k) = go p >>= (go . k)
    go (Instr i)  = runi $ imap go i

-- | Interpret a program in a monad
interpretWithMonad :: (MapInstr instr, Monad m) =>
    (forall b . instr m b -> m b) -> Program instr a -> m a
interpretWithMonad interp = interpretWithMonadT interp (return . runIdentity)

-- | @`Interp` i m@ represents the fact that @i@ can be interpreted in the monad @m@
class Interp i m
  where
    -- | Interpret an instruction in a monad
    interp :: i m a -> m a

instance (Interp i1 m, Interp i2 m) => Interp (i1 :+: i2) m
  where
    interp (Inl i) = interp i
    interp (Inr i) = interp i

-- | Interpret a program in a monad. The interpretation of primitive instructions is provided by the
-- 'MapInstr' class.
interpretT :: (Interp i m, MapInstr i, Monad m) => (forall b . n b -> m b) -> ProgramT i n a -> m a
interpretT = interpretWithMonadT interp

-- | Interpret a program in a monad. The interpretation of primitive instructions is provided by the
-- 'MapInstr' class.
interpret :: (Interp i m, MapInstr i, Monad m) => Program i a -> m a
interpret = interpretWithMonad interp

