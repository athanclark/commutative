{-# LANGUAGE
    GeneralizedNewtypeDeriving
  #-}

module Data.Commutative where

import Data.Monoid (Any (..), All (..), First (..), Last (..))
import System.IO.Unsafe (unsafePerformIO)
import System.Random (randomIO)


class Commutative a where
  commute :: a -> a -> a -- ^ Abelian binary operation - @x `commute` y == y `commute` x@

(<~>) :: Commutative a => a -> a -> a
(<~>) = commute

class Commutative a => CommutativeId a where
  cempty :: a -- ^ Identity element - @x `commute` cempty == cempty `commute` x == x@


-- | Endomorphisms commutative over composition.
newtype CommEndo a = CommEndo {appCommEndo :: a -> a}

instance Commutative (CommEndo a) where
  commute (CommEndo f) (CommEndo g) = CommEndo $ pick1 (f . g) (g . f) -- it doesn't matter which is chosen

instance CommutativeId (CommEndo a) where
  cempty = CommEndo id

-- Booleans
instance Commutative Any where
  commute (Any x) (Any y) = Any $ x || y

instance CommutativeId Any where
  cempty = Any False

instance Commutative All where
  commute (All x) (All y) = All $ x && y

instance CommutativeId All where
  cempty = All True

-- Maybe
newtype OneOf a = OneOf {getOneOf :: Maybe a}
  deriving (Show, Eq)

instance Commutative (OneOf a) where
  commute (OneOf x) (OneOf y) = OneOf $ pick1 (getFirst $ First x `mappend` First y)
                                              (getLast  $ Last x  `mappend` Last y)


pick1 :: a -> a -> a
pick1 l r = let leftOrRight = unsafePerformIO randomIO
            in if leftOrRight then l else r
