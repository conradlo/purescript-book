module Exercise where

import Prelude

import Data.Array ((:))
import Data.Foldable (class Foldable, foldr, foldl)

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