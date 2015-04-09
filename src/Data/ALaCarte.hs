{-# LANGUAGE CPP #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE PolyKinds #-}

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

instance (f :<: f)
  where
    inj = id
    prj = Just

instance (f :<: (f :+: g))
  where
    inj = Inl
    prj (Inl f) = Just f
    prj _       = Nothing

instance (f :<: h) => (f :<: (g :+: h))
  where
    inj = Inr . inj
    prj (Inr h) = prj h
    prj _       = Nothing

