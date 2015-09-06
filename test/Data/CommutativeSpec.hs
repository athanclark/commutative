module Data.CommutativeSpec (spec) where

import Data.Commutative
import Data.Mergeable
import Data.Monoid
import Data.Maybe (isJust)
import Data.List (permutations)
import Control.Applicative

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck
import Test.QuickCheck.Instances


spec :: [TestTree]
spec =
  [ testGroup "Commutative Operation"
    [ QC.testProperty "Unit `()`  should commute" (commutes :: () -> () -> Bool)
    , QC.testProperty "`Any`      should commute" (commutes :: Any -> Any -> Bool)
    , QC.testProperty "`All`      should commute" (commutes :: All -> All -> Bool)
    , QC.testProperty "`OneOf ()` should commute" (commutes :: OneOf () -> OneOf () -> Bool)
    , QC.testProperty "`Sum`      should commute" (commutes :: Sum Int -> Sum Int -> Bool)
    , QC.testProperty "`Product`  should commute" (commutes :: Product Int -> Product Int -> Bool)
    ]
  , testGroup "Left / Right Identity"
    [ QC.testProperty "Unit `()`  should have id" (lridentity :: () -> Bool)
    , QC.testProperty "`Any`      should have id" (lridentity :: Any -> Bool)
    , QC.testProperty "`All`      should have id" (lridentity :: All -> Bool)
    , QC.testProperty "`OneOf ()` should have id" (lridentity :: OneOf () -> Bool)
    , QC.testProperty "`Sum`      should have id" (lridentity :: Sum Int -> Bool)
    , QC.testProperty "`Product`  should have id" (lridentity :: Product Int -> Bool)
    ]
  , testGroup "Mergeable"
    [ QC.testProperty "Lists should merge" (merges :: Under10 (Sum Int) -> Bool )]
  ]

commutes :: (Eq a, Commutative a) => a -> a -> Bool
commutes x y = x <~> y == y <~> x

lridentity :: (Eq a, CommutativeId a) => a -> Bool
lridentity x = x <~> cempty == x
            && cempty <~> x == x

merges :: (Eq a, CommutativeId a) => Under10 a -> Bool
merges (Under10 xs) = let merge' = merge (<~>) cempty
                      in equal $ map merge' $ permutations xs

-----------------------------

newtype Under10 a = Under10 {unUnder10 :: [a]}
  deriving (Show, Eq)

instance Arbitrary a => Arbitrary (Under10 a) where
  arbitrary = Under10 <$> arbitrary `suchThat` (\x -> length x < 10)

instance Arbitrary Any where
  arbitrary = Any <$> arbitrary

instance Arbitrary All where
  arbitrary = All <$> arbitrary

instance Arbitrary a => Arbitrary (OneOf a) where
  arbitrary = OneOf <$> arbitrary

instance Arbitrary a => Arbitrary (Sum a) where
  arbitrary = Sum <$> arbitrary

instance Arbitrary a => Arbitrary (Product a) where
  arbitrary = Product <$> arbitrary

equal :: (Eq a, Foldable f) => f a -> Bool
equal = maybe True snd
      . foldr (\a mb -> Just $ maybe (a, True)
                  (\(b, p) -> (b, p && (a == b))) mb) Nothing
