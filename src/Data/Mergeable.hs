module Data.Mergeable where

import Data.Commutative


class Mergeable t where
  mergeMap :: CommutativeId m => (a -> m) -> t a -> m
  merge :: (a -> b -> b) -> b -> t a -> b

  mergeMap f = merge (commute . f) cempty
  merge f i xs = appCommEndo (mergeMap (CommEndo . f) xs) i
