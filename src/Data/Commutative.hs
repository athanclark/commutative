module Data.Commutative where




class Commutative a where
  commute :: a -> a -> a -- ^ Abelian binary operation - @x `commute` y == y `commute` x@
