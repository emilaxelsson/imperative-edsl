{-# LANGUAGE CPP #-}
{-# LANGUAGE PolyKinds #-}

#ifndef MIN_VERSION_GLASGOW_HASKELL
#define MIN_VERSION_GLASGOW_HASKELL(a,b,c,d) 0
#endif
  -- MIN_VERSION_GLASGOW_HASKELL was introduced in GHC 7.10

#if MIN_VERSION_GLASGOW_HASKELL(7,10,0,0)
#else
{-# LANGUAGE OverlappingInstances #-}
#endif

-- | Higher-order (and poly-kinded) implementation of Data Types à la Carte [1]
--
-- \[1\] W. Swierstra. Data Types à la Carte.
--       /Journal of Functional Programming/, 18(4):423-436, 2008,
--       <http://dx.doi.org/10.1017/S0956796808006758>.

module Data.ALaCarte where



-- | Coproducts
data (f :+: g) a b
    = Inl (f a b)
    | Inr (g a b)
#if  __GLASGOW_HASKELL__>=708
  deriving (Functor)
#endif

infixr :+:

-- | A constraint @f `:<:` g@ expresses that the signature @f@ is subsumed by @g@, i.e. @f@ can be
-- used to construct elements in @g@.
class f :<: g
  where
    inj :: f a b -> g a b
    prj :: g a b -> Maybe (f a b)

instance {-# OVERLAPPING #-} (f :<: f)
  where
    inj = id
    prj = Just

instance {-# OVERLAPPING #-} (f :<: (f :+: g))
  where
    inj = Inl
    prj (Inl f) = Just f
    prj _       = Nothing

instance {-# OVERLAPPING #-} (f :<: h) => (f :<: (g :+: h))
  where
    inj = Inr . inj
    prj (Inr h) = prj h
    prj _       = Nothing

-- | Higher-order functors
class HFunctor h
  where
    -- | Higher-order 'fmap'
    hfmap :: (forall b . m b -> n b) -> h m a -> h n a

instance (HFunctor h1, HFunctor h2) => HFunctor (h1 :+: h2)
  where
    hfmap f (Inl i) = Inl (hfmap f i)
    hfmap f (Inr i) = Inr (hfmap f i)

