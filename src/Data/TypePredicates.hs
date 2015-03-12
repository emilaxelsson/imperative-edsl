{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Utilities for working with type predicates (i.e. types of kind @* -> `Constraint`@)
module Data.TypePredicates where



-- | Intersection of type predicates
class    (p a, q a) => (p :/\: q) a
instance (p a, q a) => (p :/\: q) a

infixr 5 :/\:

-- | Universal type predicate
class    Any a
instance Any a

