{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Utilities for working with type predicates (i.e. types of kind @* -> `Constraint`@)
module Data.TypePredicates
  ( Dict (..)
  , (:/\:)
  , Any
  , Subsumes
  , weakL
  , weakR
  , (:<) (..)
  ) where



import Data.Constraint



-- | Intersection of type predicates
class    (p a, q a) => (p :/\: q) a
instance (p a, q a) => (p :/\: q) a

infixr 5 :/\:

-- | Universal type predicate
class    Any a
instance Any a

-- | Evidence that the predicate @p@ subsumes @q@
type Subsumes p q = forall a . Dict (p a) -> Dict (q a)

-- | Weaken an intersection
weakL :: Subsumes (p :/\: q) p
weakL Dict = Dict

-- | Weaken an intersection
weakR :: Subsumes (p :/\: q) q
weakR Dict = Dict

-- | A constraint @p `:<` q@ denotes that @p@ is subsumed by @q@
class p :< q
  where
    -- | Compute evidence that @p@ subsumes @q@
    sub :: Subsumes q p

instance p :< p
  where
    sub = id

instance p :< (p :/\: ps)
  where
    sub = weakL

instance (p :< qs) => (p :< (q :/\: qs))
  where
    sub = sub . weakR

