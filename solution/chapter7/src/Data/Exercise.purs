module Exercise where

import Prelude

import Control.Apply (lift2)
import Data.Foldable (class Foldable, foldr, foldl)
import Data.Maybe (Maybe(..))
import Data.Traversable (class Traversable, traverse, sequence)

-- 7.8 Ex 1
{-
  Use `lift2` to write lifted versions of the numeric operators `+`, `-`, `*` and `/`,
  which work with optional arguments
-}
add' :: Maybe Int -> Maybe Int -> Maybe Int
add' = lift2 (+)

sub' :: Maybe Int -> Maybe Int -> Maybe Int
sub' = lift2 (-)

mul' :: Maybe Int -> Maybe Int -> Maybe Int
mul' = lift2 (*)

div' :: Maybe Int -> Maybe Int -> Maybe Int
div' = lift2 (/)

-- 7.8 Ex 2
-- Convince yourself that the definition of `lift3` given above in terms of `<$>` and `<*>` does type check.

{-
  lift3 :: forall a b c d f. Apply f => (a -> b -> c -> d)
        -> f a
        -> f b
        -> f c
        -> f d
  lift3 func x y z = func <$> x <*> y <*> z
  
  1. (func <$> x) <*> y <*> z
  <$> lifted the `func` into `x`
  e.g. if x is `Maybe Int` and `func` is of type `Int -> Int -> Int -> Int`
  then ``func <$> x` is of type `Maybe (Int -> Int -> Int)`

  2. ({result of 1.} <*> y) <*> z
  then {result of 1.} which is of type `Maybe (Int -> Int -> Int)` is 'applied' to another `Maybe Int` (y)
  so ({result of 1.} <*> y) becomes: `Maybe (Int -> Int)`

  3. similarly, ({result of 2.} <*> z) becomes `Maybe Int`
  
  hence the types are correct
  `lift3` lifted a function (`func`) which is of type `Int -> Int -> Int -> Int` and create a new function
  of type `Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int`
-}

-- 7.8 Ex 3
{-
  Write a function `combineMaybe` which has type
  `forall a f. Applicative f => Maybe (f a) -> f (Maybe a)`
  
  This function takes an optional computation with side-effects,
  and returns a side-effecting computation which has an optional result.
-}
combineMaybe :: forall a f. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe Nothing = pure Nothing
combineMaybe (Just fa) = Just <$> fa

-- 7.10 Ex 1
{-
  Use a regular expression validator to ensure that the `state` field of the `Address` type
  contains two alphabetic characters.
  Hint: see the source code for `phoneNumberRegex`
-}
-- see Data.AddressBook.Validation.purs


-- 7.10 Ex 2
{-
  Using the `matches` validator, write a validation function which checks that a string is not entirely whitespace.
  Use it to replace `nonEmpty` where appropriate.
-}

-- 7.11 Ex 1
{-
  Write a `Traversable` instance for the following binary tree data structure,
  which combines side-effects from left-to-right:
-}
data Tree a = Leaf | Branch (Tree a) a (Tree a)

-- 1. to make `Traversable Tree` must first implement `Functor Tree` & `Foldable Tree`
instance mapTree :: Functor Tree where
  map :: forall a b. (a -> b) -> Tree a -> Tree b
  map _ Leaf = Leaf
  map f (Branch left a right) = Branch (map f left) (f a) (map f right)

instance foldableTree :: Foldable Tree where
  foldr :: forall a b. (a -> b -> b) -> b -> Tree a -> b
  foldr _ b Leaf = b
  foldr f b (Branch left a right) = foldr f (foldr f b left) (Branch Leaf a right)
  foldl :: forall a b. (b -> a -> b) -> b -> Tree a -> b
  foldl _ b Leaf = b
  foldl f b (Branch left a right) = foldl f (foldl f b left) (Branch Leaf a right)
  foldMap :: forall a m. Monoid m => (a -> m) -> Tree a -> m
  foldMap f Leaf = mempty
  foldMap f treeA = foldl append mempty (map f treeA)


instance traverseTree :: Traversable Tree where
  traverse :: forall a b f. Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse _ Leaf = pure Leaf
  traverse f (Branch leftChild a rightChild) = Branch <$> traverse f leftChild <*> f a <*> traverse f rightChild
  sequence :: forall a f. Applicative f => Tree (f a) -> f (Tree a)
  sequence Leaf = pure Leaf
  sequence (Branch leftChild fa rightChild) = Branch <$> sequence leftChild <*> fa <*> sequence rightChild

-- This corresponds to an in-order traversal of the tree.
-- What about a preorder traversal? What about reverse order?
-- 1. preorder traversal: process current value first, then left child then right child
-- 2. reverse order: process right child first, then current value, lastly left
-- 3. TODO How to implement? should `Functor Tree`, `Foldable Tree`, `Traversable Tree` be all different?

-- 7.11 Ex 2
{-
  Modify the code to make the `address` field of the Person type optional using `Data.Maybe`.
  Hint: Use `traverse` to validate a field of type `Maybe a`.
-}
-- see Data.AddressBook.Validation

-- 7.11 Ex 3
{-
  Try to write `sequence` in terms of `traverse`.
  Can you write `traverse` in terms of `sequence`?
-}

{- `Traversable` type class
class (Functor t, Foldable t) <= Traversable t where
  traverse :: forall a b f. Applicative f => (a -> f b) -> t a -> f (t b)
  sequence :: forall a f. Applicative f => t (f a) -> f (t a)
-}

{-
  1. Try to write `sequence` in terms of `traverse`.
  i. sub a as b
  sequence :: forall b f. Applicative f => t (f b) -> f (t b)
  ii. sub a as (f b)
  sequence :: forall b f. Applicative f => t a -> f (t b)

  there for
  sequence == traverse (a -> f b) where a is (f b) for some function (f b -> f b), i.e. identity
  
  write `sequence` in terms of `traverse`
  sequence = traverse identity
-}

{-
  2. Can you write `traverse` in terms of `sequence`?

  `traverse` takes an function (a -> f b) and apply to some `Traversable a` and gives a `f (Traversable b)`
  `sequence` takes some `Traversable (f a)` and gives `f (Traversable a)`

  if we first map a function of (a -> f b) to some `Traversable a` we get `Traversable (f b)`
  then we gives `sequence` a `Traversable (f b)` we get `f (Traversable b)` which is the same as `traverse` function

  traverse f = (sequence <<< map f)
-}