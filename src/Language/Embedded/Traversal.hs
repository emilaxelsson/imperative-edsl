{-# LANGUAGE CPP #-}

-- | Methods for traversing programs

module Language.Embedded.Traversal where



import Control.Monad.Operational.Higher

import Control.Monads



-- | Dry (effect-less) interpretation of an instruction. This class is like
-- 'Interp' without the monad parameter, so it cannot have different instances
-- for different monads.
class DryInterp instr
  where
    -- | Dry interpretation of an instruction. This function is like 'interp'
    -- except that it interprets in any monad that can supply fresh variables.
    dryInterp :: MonadSupply m => instr '(m,fs) a -> m a

-- | Interpretation of a program as a combination of dry interpretation and
-- effectful observation
observe_ :: (DryInterp instr, HFunctor instr, MonadSupply m)
    => (forall a . instr '(m,fs) a -> a -> m ())  -- ^ Function for observing instructions
    -> Program instr fs a
    -> m a
observe_ obs = interpretWithMonad $ \i -> do
    a <- dryInterp i
    obs i a
    return a

-- | Interpretation of a program as a combination of dry interpretation and
-- effectful observation
observe :: (DryInterp instr, HFunctor instr, MonadSupply m)
    => (forall a . instr '(m,fs) a -> a -> m a)  -- ^ Function for observing instructions
    -> Program instr fs a
    -> m a
observe obs = interpretWithMonad $ \i -> do
    a <- dryInterp i
    obs i a

instance (DryInterp i1, DryInterp i2) => DryInterp (i1 :+: i2)
  where
    dryInterp (Inl i) = dryInterp i
    dryInterp (Inr i) = dryInterp i

