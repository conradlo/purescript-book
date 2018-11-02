module Exercise where

import Prelude

import Data.Array as Array
import Data.Int (even)
import Data.List as List
import Data.Maybe (Maybe(..))

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
  nofirst <- Array.tail array
  nosecond <- Array.tail nofirst
  Array.head nosecond

-- 8.7 Ex 2

sums :: Array Int -> Array Int
sums arr = (Array.sort <<< Array.nub <<< Array.foldM (\x y -> [x, y, x + y]) 0) arr

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

-- 8.7 Ex 5

{-
  Write a function `filterM` which generalizes the `filter` function on lists.
  Your function should have the following type signature:
-}

{-
-- ver1: use foldM
filterM :: forall m a. Monad m => (a -> m Boolean) -> List.List a -> m (List.List a)
filterM _ List.Nil = pure List.Nil
filterM fn l = List.foldM filtering List.Nil l where
  filtering la a = do
    pass <- fn a
    if pass then pure (List.Cons a la) else pure la
-}

{-
-- ver2: use recursion
filterM :: forall m a. Monad m => (a -> m Boolean) -> List.List a -> m (List.List a)
filterM = filterM' List.Nil where
  -- filterM' :: List.List a -> (a -> m Boolean) -> List.List a -> m (List.List a)
  filterM' result _ List.Nil  = pure result
  filterM' result fn' (List.Cons x xs) = do
    pass <- fn' x
    if pass then
      filterM' (List.Cons x result) fn' xs
      else filterM' result fn' xs 
-}

-- ver3: use applicative and recursion
filterM :: forall m a. Monad m => (a -> m Boolean) -> List.List a -> m (List.List a)
filterM _ List.Nil = pure List.Nil
filterM fn (List.Cons x xs) = do
  pass <- fn x
  if pass then
    List.Cons <$> pure x <*> (filterM fn xs)
    else filterM fn xs

--  testing
safeDivide :: Int -> Int -> Maybe Int
safeDivide _ 0 = Nothing
safeDivide a b = Just (a / b)

predicate :: Int -> Maybe Boolean
predicate i = even <$> safeDivide i (i - 3)