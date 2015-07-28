module Data.Commutative where

import Data.Monoid (Endo (..))


class Commutative a where
  commute :: a -> a -> a -- ^ Abelian binary operation - @x `commute` y == y `commute` x@

(<~>) :: Commutative a => a -> a -> a
(<~>) = commute

class Commutative a => CommutativeId a where
  cempty :: a -- ^ Identity element - @x `commute` cempty == cempty `commute` x == x@


instance Commutative (Endo a) where
  commute (Endo f) (Endo g) = pick1 (f . g) (g . f) -- it doesn't matter which is chosen
