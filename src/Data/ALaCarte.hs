{-# LANGUAGE OverlappingInstances #-}

module Data.ALaCarte where



data (f :+: g) a
    = Inl (f a)
    | Inr (g a)
  deriving (Functor)

infixr :+:

class f :<: g
  where
    inj :: f a -> g a

instance (f :<: f)
  where
    inj = id

instance (f :<: (f :+: g))
  where
    inj = Inl

instance (f :<: h) => (f :<: (g :+: h))
  where
    inj = Inr . inj

