module Exercise where

import Prelude

import Data.Array ((:), nubByEq, length)
import Data.Foldable (class Foldable, foldr, foldl, maximum)
import Data.Hashable (class Hashable, hashEqual, hashCode)
import Data.Maybe (fromJust)

-- 6.4 Ex 1
-- The following newtype represents a complex number:
newtype Complex = Complex {
  real :: Number,
  imaginary :: Number  
}
-- Define Show and Eq instances for Complex.
instance showComplex :: Show Complex where
  show (Complex { real, imaginary }) = "Complex real: " <> show real <> " imaginary: " <>  show imaginary

instance eqComplex :: Eq Complex where
  eq (Complex i1) (Complex i2) = i1.real == i2.real && i1.imaginary == i2.imaginary


-- 6.7 Ex 1
-- The following declaration defines a type of non-empty arrays of elements of type `a`:
data NonEmpty a = NonEmpty a (Array a)

instance eqNonEmpty :: (Eq a, Eq (Array a)) => Eq (NonEmpty a) where
  eq (NonEmpty a1 arr1) (NonEmpty a2 arr2) = a1 == a2 && arr1 == arr2

instance showNonEmpty :: (Show a, Show (Array a)) => Show (NonEmpty a) where
  show (NonEmpty el arrEl) = "NonEmpty : " <> show el <> " Array: " <> show arrEl

-- 6.7 Ex 2
-- Write a `Semigroup` instance for `NonEmpty a` by reusing the `Semigroup` instance for `Array`.
instance appendNonEmpty :: (Semigroup (Array a)) => Semigroup (NonEmpty a) where
  append (NonEmpty el1 arr1) (NonEmpty el2 arr2) = NonEmpty el1 (el2 : arr1 <> arr2)

-- class Functor f where
--   map :: forall a b. (a -> b) -> f a -> f b

-- 6.7 Ex 3
-- Write a `Functor` instance for `NonEmpty`.
instance mapNonEmpty :: Functor (NonEmpty) where
  map func (NonEmpty el arr) = NonEmpty (func el) (map func arr)

-- 6.7 Ex 4
-- Write an `Ord` instance for `Extended a` which reuses the `Ord` instance for `a`.
data Extended a = Finite a | Infinite

instance showExtended :: (Show a) => Show (Extended a) where
  show :: forall a. (Show a) => Extended a -> String
  show ex =  case ex of
    Infinite -> "Infinite"
    Finite a -> "Finite " <> show a 

instance eqExtended :: Eq a => Eq (Extended a) where
  eq :: forall a. Eq a => Extended a -> Extended a -> Boolean
  eq Infinite Infinite = true
  eq (Finite a) (Finite b) = eq a b
  eq _ _ = false

instance compareExtended :: Ord a => Ord (Extended a) where
  compare :: forall a. Ord a => Extended a -> Extended a -> Ordering
  compare Infinite Infinite = EQ
  compare Infinite _ = GT
  compare _ Infinite = LT
  compare (Finite a) (Finite b) = compare a b

-- 6.7 Ex 5
-- Write a `Foldable` instance for `NonEmpty`. Hint: reuse the `Foldable` instance for arrays.
{-
  class Foldable f where
    foldr :: forall a b. (a -> b -> b) -> b -> f a -> b
    foldl :: forall a b. (b -> a -> b) -> b -> f a -> b
    foldMap :: forall a m. Monoid m => (a -> m) -> f a -> m
-}
-- instance memptyNonEmpty :: Monoid a => Monoid (NonEmpty a) where
--   mempty = NonEmpty mempty []

instance foldableNonEmpty :: Foldable (NonEmpty) where
  foldr :: forall a b. (a -> b -> b) -> b -> NonEmpty a -> b
  foldr func init_b (NonEmpty el arr) = foldr func (func el init_b) arr
  foldl :: forall a b. (b -> a -> b) -> b -> NonEmpty a -> b
  foldl func init_b (NonEmpty el arr) = foldl func (func init_b el) arr
  foldMap :: forall a m. Monoid m => (a -> m) -> NonEmpty a -> m
  foldMap func (NonEmpty a arr) = foldl append mempty (map func (NonEmpty a arr))

-- 6.7 Ex 6
{-
  Given a type constructor `f` which defines an ordered container
  (and so has a `Foldable` instance), we can create a new container type
  which includes an extra element at the front:
-}

data OneMore f a = OneMore a (f a)

{-
  The container `OneMore f` is also has an ordering,
  where the new element comes before any element of `f`.
  Write a `Foldable` instance for `OneMore f`: 
-}

-- to support `foldMap`, `f` should be a Functor as well (?)
instance mapOneMore :: Functor f => Functor (OneMore f) where
  map :: forall f a b. Functor f => (a -> b) -> OneMore f a -> OneMore f b
  map func (OneMore a fa) = OneMore (func a) (map func fa)

instance foldableOneMore :: (Functor f, Foldable f) => Foldable (OneMore f) where
  foldr :: forall f a b. Foldable f => (a -> b -> b) -> b -> OneMore f a -> b
  foldr func init_b (OneMore a fa) = foldr func (func a init_b) fa
  foldl :: forall f a b. Foldable f => (b -> a -> b) -> b -> OneMore f a -> b
  foldl func init_b (OneMore a fa) = foldl func (func init_b a) fa
  foldMap :: forall f a m. Functor f => Foldable f => Monoid m => (a -> m) -> OneMore f a -> m
  foldMap func (OneMore a fa) = foldl append mempty (map func (OneMore a fa))


-- 6.11 Ex 1
{- 
  Define a partial function which finds the maximum of a non-empty array of integers.
  Your function should have type `Partial => Array Int -> Int`.
  Test out your function in PSCi using `unsafePartial`. Hint: Use the `maximum` function from `Data.Foldable`.
-}
maxInt :: Partial => Array Int -> Int
maxInt xs = fromJust $ maximum xs

-- 6.11 Ex 2

class Monoid m <= Action m a where
  act :: m -> a -> a

newtype Multiply = Multiply Int

instance semigroupMultiply :: Semigroup Multiply where
  append (Multiply n) (Multiply m) = Multiply (n * m)

instance monoidMultiply :: Monoid Multiply where
  mempty = Multiply 1

instance repeatAction :: Action Multiply String where
  act :: Multiply -> String -> String
  act (Multiply n) str | n < 1 = ""
                       | otherwise = str <> act (Multiply (n - 1)) str

{-
  Action laws:
  1. act mempty a = a
  2. act (m1 <> m2) a = act m1 (act m2 a)

  1st law holds
  mempty = Multiply 1:
  act (Multiply 1) "string" == "string"

  if n, m > 0
  set m1 = Multiply 2, m2 = Multiply 3:
  act (Multiply 2 <> Multiply 3) "<string>"
    == act (Multiply 2) (act (Multiply 3) "<string>")
    == "<string><string><string><string><string><string>"

  what if '0' or negative Int in `Multiply`?

  if n == 0, forall m
  act (Multiply 0 <> Multiply m) "<string>"
    == act (Multiply 0) "<string>"
    == ""
  act (Multiply 0) (act (Multiply m) "<string>") == ""
  act (Multiply m) (act (Multiply 0) "<string>") == ""

  if n, m < 0
  act (Multiply -2 <> Multiply -3) "<string>" == act (Multiply 6) "<string>"
  /=
  act (Multiply -2) (act (Multiply -3) "<string>") == ""

  Does this instance satisfy the laws listed above? Ans: Yes iff n,m not both negative forall (Multiply n) (Multiply m)
-}

-- 6.11 Ex 3
{-
 Write an instance `Action m a => Action m (Array a)`
 where the action on arrays is defined by acting on each array element independently.
-}
instance actOnArray :: Action m a => Action m (Array a) where
  act :: m -> Array a -> Array a
  act m = map (act m)

-- 6.11 Ex 4
{-
  Given the following newtype, write an instance for `Action m (Self m)`
  where the monoid `m` acts on itself using `append`:
-}
newtype Self m = Self m
instance actOnSelf :: Monoid m => Action m (Self m) where
  act :: m -> Self m -> Self m
  act m (Self n) = Self (append m n)

-- 6.11 Ex 5
{-
  Should the arguments of the multi-parameter type class `Action`
  be related by some functional dependency? Why or why not?
-}

{-
  Ans:
    No. given a `m` of type `Monoid` doesn't automactically indicated the type of `a`
    In fact, the above example shows given the same type (Monoid m), the `act` function
    can return (and accept) different types of `a`
      e.g.
        given (Monoid m) -> Array a -> Array a, here a is an `Array a`
        given (Monoid m) -> Self m -> Self m, here a is an `Self m`
  
  moreover, if added functional dependency e.g.
  ```
  class Monoid m <= Action m a | m -> a where
    act :: m -> a -> a
  ```
  compiler shows "Overlapping type class instances" errors on any 2 of the following instances
    Exercise.repeatAction
    Exercise.actOnArray
    Exercise.actOnSelf
  
-}

-- 6.12 Ex 2
{-
  Use the `hashEqual` function to write a function which tests if an array has any duplicated elements,
  using hash-equality as an approximation to value equality.
  
  Remember to check for value equality using `==` if a duplicate pair is found.
  Hint: the `nubBy` function in `Data.Array` should make this task much simpler.
-}

removeDuplicate :: forall a. Hashable a => Array a -> Array a
removeDuplicate = nubByEq hashEqual

hasDuplicate :: forall a. Hashable a => Array a -> Boolean
hasDuplicate arr = (length $ removeDuplicate arr) < (length arr)


-- 6.12 Ex 3
-- Write a `Hashable` instance for the following newtype which satisfies the type class law:

newtype Hour = Hour Int
  
instance eqHour :: Eq Hour where
  eq (Hour n) (Hour m) = mod n 12 == mod m 12

{-
  The newtype Hour and its Eq instance represent the type of integers modulo 12,
  so that 1 and 13 are identified as equal, for example.
  Prove that the type class law holds for your instance.
-}

instance hashHour :: Hashable Hour where
  hash (Hour h) = hashCode (mod h 12)
{-
(Hour 1) == (Hour 13) : true
hash (Hour 1) == hash (Hour 13) : true
-}

-- 6.12 Ex 4
-- Prove the type class laws for the `Hashable` instances for `Maybe`, `Either` and `Tuple`.

{-

- Maybe
hash Nothing == hash Nothing , trivial
hash (Just a) == hash (Just b) iff hash a == hash b for some a == b, i.e. a, b are Hashable
since a, b are Hashable becos `hashMaybe :: Hashable a => Hashable (Maybe a)`,
the law holds

- Tuple
hash (Tuple a b) == hash (Tuple c d) iff hash a == hash c, hash b == hash d, for some a == c && b == d
the law holds since `combineHashes` is deterministic. hashTuple : hash (Tuple a b) = hash a `combineHashes` hash b

- Either
  hash (Left a) == hash (Left b) iff hash a == hash b for some a == b, since `combineHashes` is deterministic.
  in `hashEither`, case of hash (Left a) = hashCode 0 `combineHashes` hash a
  the `Left` side holds

  hash (Left c) == hash (Left d) iff hash c == hash d for some c == d, since `combineHashes` is deterministic.
  in `hashEither`, case of hash (Right b) = hashCode 1 `combineHashes` hash b
  the `Right` side holds

  therefore, the law holds for `Either a b` for Hashable a, b
-}