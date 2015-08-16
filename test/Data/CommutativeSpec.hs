module Data.CommutativeSpec (spec) where

import Data.Commutative
import Data.Monoid

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
  ]

commutes :: (Eq a, Commutative a) => a -> a -> Bool
commutes x y = x <~> y == y <~> x

lridentity :: (Eq a, CommutativeId a) => a -> Bool
lridentity x = x <~> cempty == x
            && cempty <~> x == x

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
