module Data.Commutative where




class Commutative a where
  commute :: a -> a -> a -- ^ Abelian binary operation - @x `commute` y == y `commute` x@

(<~>) :: Commutative a => a -> a -> a
(<~>) = commute

class Commutative a => CommutativeId a where
  cempty :: a -- ^ Identity element - @x `commute` cempty == cempty `commute` x == x@
