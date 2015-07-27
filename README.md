commutative
===========

> Semantics for commutative operations in Haskell

## Major Issues

- `Semigroup` isn't a superclass to `Monoid`
    - if it were, then `Magma a => Commutative a` would be straightforward for
      monomorphic, commutative binary operations via `<~>`.

## Foldable Concerns

- "Is every `Foldable` `CommutativeFoldable`?

- If `concat` ~ `foldr1 <>`, then `total` ~ `foldr1 <~>`.

- If `foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m`, then `foldMapC :: (CommutativeMonoid m, CommutativeFoldable t) => (a -> m) -> t a -> m`

- Unordered Sets
