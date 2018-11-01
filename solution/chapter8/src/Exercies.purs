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
