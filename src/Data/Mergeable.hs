module Data.Mergeable where

import Data.Commutative
import Data.Monoid


class Mergeable t where
  mergeMap :: CommutativeId m => (a -> m) -> t a -> m
  merge :: (a -> b -> b) -> b -> t a -> b

  mergeMap f = merge (commute . f) cempty
  merge f i xs = appEndo (mergeMap (Endo #. f) xs) i
