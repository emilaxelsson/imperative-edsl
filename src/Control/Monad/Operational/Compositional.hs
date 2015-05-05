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
    ( module Control.Monad
    , module Data.ALaCarte
      -- * Program monad
    , ProgramT
    , Program
    , singleton
    , singleInj
      -- * Interpretation
    , MapInstr (..)
    , liftProgram
    , interpretWithMonadT
    , interpretWithMonad
    , Interp (..)
    , interpretT
    , interpret
    , ProgramViewT (..)
    , ProgramView (..)
    , viewT
    , view
    , unview
      -- * Traversing programs
    , DryInterp (..)
    , observe_
    , observe
    , fresh
    , freshStr
      -- * Working with instruction sets
    , IExp
    , injE
    , prjE
    , singleE
    ) where



import Control.Applicative (Applicative (..))
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monads
import Data.Typeable

import Data.ALaCarte



----------------------------------------------------------------------------------------------------
-- * Program monad
----------------------------------------------------------------------------------------------------

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



----------------------------------------------------------------------------------------------------
-- * Interpretation
----------------------------------------------------------------------------------------------------

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

-- | View type for inspecting the first instruction
data ProgramViewT instr m a
  where
    Return :: a -> ProgramViewT instr m a
    (:>>=) :: instr (ProgramT instr m) b -> (b -> ProgramT instr m a) -> ProgramViewT instr m a

-- | View type for inspecting the first instruction
type ProgramView instr = ProgramViewT instr Identity

-- | View function for inspecting the first instruction
viewT :: Monad m => ProgramT instr m a -> m (ProgramViewT instr m a)
viewT (Lift m)                = m >>= return . Return
viewT (Lift m       `Bind` g) = m >>= viewT . g
viewT ((m `Bind` g) `Bind` h) = viewT (m `Bind` (\x -> g x `Bind` h))
viewT (Instr i      `Bind` g) = return (i :>>= g)
viewT (Instr i)               = return (i :>>= return)

-- | View function for inspecting the first instruction
view :: MapInstr instr => Program instr a -> ProgramView instr a
view = runIdentity . viewT

-- | Turn a 'ProgramViewT' back to a 'Program'
unview :: Monad m => ProgramViewT instr m a -> ProgramT instr m a
unview (Return a) = return a
unview (i :>>= k) = singleton i >>= k



----------------------------------------------------------------------------------------------------
-- * Traversing programs
----------------------------------------------------------------------------------------------------

-- | Dry (effect-less) interpretation of an instruction. This class is like 'Interp' without the
-- monad parameter, so it cannot have different instances for different monads.
class DryInterp instr
  where
    -- | Dry interpretation of an instruction. This function is like 'interp' except that it
    -- interprets in any monad that can supply fresh variables.
    dryInterp :: MonadSupply m => instr m a -> m a

-- | Interpretation of a program as a combination of dry interpretation and effectful observation
observe_ :: (DryInterp instr, MapInstr instr, MonadSupply m)
    => (forall a . instr m a -> a -> m ())  -- ^ Function for observing instructions
    -> Program instr a
    -> m a
observe_ obs = interpretWithMonad $ \i -> do
    a <- dryInterp i
    obs i a
    return a

-- | Interpretation of a program as a combination of dry interpretation and effectful observation
observe :: (DryInterp instr, MapInstr instr, MonadSupply m)
    => (forall a . instr m a -> a -> m a)  -- ^ Function for observing instructions
    -> Program instr a
    -> m a
observe obs = interpretWithMonad $ \i -> do
    a <- dryInterp i
    obs i a

instance (DryInterp i1, DryInterp i2) => DryInterp (i1 :+: i2)
  where
    dryInterp (Inl i) = dryInterp i
    dryInterp (Inr i) = dryInterp i



----------------------------------------------------------------------------------------------------
-- * Working with instruction sets
----------------------------------------------------------------------------------------------------

-- | Extract the expression type from an instruction set. 'IExp' is needed to avoid types like
-- @(`SomeInstr` exp `:<:` i) => `Program` i ()@. Here it is not possible to constrain @exp@ by
-- constraining @i@, so the instance search will always fail. Functions like 'injE' solve this by
-- using 'IExp' to determine @exp@ from @i@. For this to work, one must use an instruction set @i@
-- that has an instance of 'IExp'. The instructions defined in this package provide 'IExp' instances
-- of the form
--
-- > type instance IExp (SomeInstr exp)       = exp
-- > type instance IExp (SomeInstr exp :+: i) = exp
--
-- Which means that functions like 'injE' will determine the expression type from the first summand.
-- If some other behavior is needed, it is of course also possible to make custom instruction types
-- with custom instances of 'IExp'.
type family IExp (i :: (* -> *) -> * -> *) :: * -> *

-- | Inject an instruction that is parameterized by an expression type
injE :: (i (IExp instr) :<: instr) => i (IExp instr) m a -> instr m a
injE = inj

-- | Project an instruction that is parameterized by an expression type
prjE :: (i (IExp instr) :<: instr) => instr m a -> Maybe (i (IExp instr) m a)
prjE = prj

-- | Create a program from an instruction that is parameterized by an expression type
singleE :: (i (IExp instr) :<: instr) => i (IExp instr) (ProgramT instr m) a -> ProgramT instr m a
singleE = singleton . inj

