module Exercise where

import Prelude

import Control.Apply (lift2)
import Data.Maybe (Maybe(..))

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