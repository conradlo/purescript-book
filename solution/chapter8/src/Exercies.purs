module Exercise where

import Prelude

import Data.Array (head, tail, foldM, nub, sort)
import Data.Maybe (Maybe)

-- 8.7 Ex 1
{-
  Look up the types of the `head` and `tail` functions from the `Data.Array`
  module in the `purescript-arrays` package.
  Use do notation with the `Maybe` monad to combine these functions into a function `third` 
  which returns the third element of an array with three or more elements.
  Your function should return an appropriate `Maybe` type.
-}
third :: forall a. Array a -> Maybe a
third array = do
  nofirst <- tail array
  nosecond <- tail nofirst
  head nosecond

-- 8.7 Ex 2

sums :: Array Int -> Array Int
sums arr = (sort <<< nub <<< foldM (\x y -> [x, y, x + y]) 0) arr

-- 8.7 Ex 3
{-
 apply :: forall a b f. Apply f => f (a -> b) -> f a -> f b
 ap :: forall m a b. Monad m => m (a -> b) -> m a -> m b

 since Monad is Applicative functor, `apply` maybe specifialzed to
 apply :: forall a b m. Monad m => m (a -> b) -> m a -> m b
 which is the same as `ap`
-}

-- 8.7 Ex 4
{-
  Verify that the monad laws hold for the `Monad` instance for the `Maybe` type,
  as defined in the `purescript-maybe` package.
-}

{-
  `Monad` type class definition
  `class (Applicative m, Bind m) <= Monad m`

  laws for monad:
  1. Left Identity: `pure x >>= f = f x`
  2. Right Identity: `x >>= pure = x`
  3. Applicative Superclass: `apply = ap`

  1. check Left Identity

  ```
    instance bindMaybe :: Bind Maybe where
      bind (Just x) k = k x
      bind Nothing  _ = Nothing
  ```
  ```
    instance applicativeMaybe :: Applicative Maybe where
      pure = Just
  ```
  LHS: `pure x >>= f`
  equal `bind (pure x) f`
  equal `bind (Just x) f`
  equal `f x` (from definition)
  equal == RHS

  2. check Right Identity (actually don't know the ans)
  `x >>= pure = x`
  equal bind x pure
  for some primitive value x, pure x == x

  3. checked in ex. 3

-}