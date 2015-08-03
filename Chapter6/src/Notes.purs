module Notes where

import Prelude hiding ((<#>))

import Data.Maybe
import Data.Tuple
import Data.Either
import Data.String
import Data.Char

import Data.Function

-- class Show a where 
--  show :: a -> String

{--
-- Type class instance names are defined to aid the readibility of the javascript.
-- the Boolean type belongs to the Show type class.
instance showBoolean :: Show Boolean where
  show true = "true"
  show false = "false"
  --}

{--
show $ Tuple 1 true
show $ Just "testing"
show $ Left 10
-- There is a Show for Data.Either, but psci can't infer the type.
show (Left 10 :: Either Number String)
show $ \n -> n + 1
--}

-- Exercises 6.3:
-- 1. (Easy) Use the showShape function from the previous chapter to define a Show instance for the Shape type.
{-- see Chapter5/src/Data/PatternsNotes.purs --}


-- 6.4 Some common type classes
{--
-- Eq (==) and (/=)
class Eq a where
    (==) :: a -> a -> Boolean
    (/=) :: a -> a -> Boolean

-- Ord
data Ordering = LT | EQ | GT

class (Eq a) <= Ord a where
    compare :: a -> a -> Ordering

compare 1 2
compare "z" "a"
compare 2 2

-- Num
class Num a where
    (+) :: a -> a -> a
    (-) :: a -> a -> a
    (*) :: a -> a -> a
    (/) :: a -> a -> a
    (%) :: a -> a -> a
    negate :: a -> a

-- Semigroups & Monoids!

-- Concatenation operator:
class Semigroup where
    (<>) :: a -> a -> a

class (Semigroup m) <= Monoid m where
    mempty :: m

import Data.Monoid
import Data.Foldable
foldl (<>) mempty ["Hello", " ", "World"]
foldl (<>) mempty [[1, 2, 3], [4], [5, 6]]

-- Foldable
-- Monoid describes the result of a fold, Foldable describes the type constructors of things that can be the used as a source for a fold.
-- Foldable abstracts the concept of an ordered container.

-- This can be specialized where f is Array, so you can replace f a with Array a
class Foldable f where
    foldl :: forall a b. (a -> b -> a) -> b -> f a -> b
    foldr :: forall a b. (b -> a -> b) -> b -> f a -> b
    foldMap :: forall a m. (Monoid m)  => (a -> m) -> f a -> m

foldMap show [1, 2, 3, 4, 5]

-- Functor
-- e.g. the "lifting" operator: <$>

class Functor f where
    (<$>) :: forall a b. (a -> b) -> f a -> f b

--}

-- 6.4 Exercises
-- 6.4.1 (Easy) The following newtype represents a complex number:
newtype Complex = Complex
    { real :: Number
    , imaginary :: Number
    }
-- Define Show and Eq instances for Complex.

instance showComplex :: Show Complex where
    show (Complex {real = r, imaginary = i}) = (show r) ++ " + " ++ (show i) ++ "i"

{--
show (Complex {real: 10, imaginary: 3})
--}

instance eqComplex :: Eq Complex where
    (==) (Complex a) (Complex b) = a.real == b.real && a.imaginary == b.imaginary
    (/=) (Complex a) (Complex b) = a.real /= b.real || a.imaginary /= b.imaginary

{-- true false false true:
(Complex {real: 10, imaginary: 3}) == (Complex {real: 10, imaginary: 3})
(Complex {real: 10, imaginary: 3}) /= (Complex {real: 10, imaginary: 3})
(Complex {real: 10, imaginary: 4}) == (Complex {real: 10, imaginary: 3})
(Complex {real: 10, imaginary: 3}) /= (Complex {real: 1, imaginary: 3})
--}

-- 6.4.2 (Medium) The following type defines a type of non-empty arrays of elements of type a:

data NonEmpty a = NonEmpty a [a]

-- Write a Semigroup instance for non-empty arrays by reusing the Semigroup instance for [].

instance semigroupNonEmpty :: Semigroup (NonEmpty a) where
    (<>) (NonEmpty a b) (NonEmpty c d) = NonEmpty a (b <> [c] <> d)

instance showNonEmpty :: Show (NonEmpty Number) where
    show (NonEmpty a b) = show $ [a] ++ b

{-- concatNonEmpty :: forall a. (NonEmpty a) -> (NonEmpty a) -> (NonEmpty a) --}
{-- concatNonEmpty (NonEmpty a b) (NonEmpty c d) = NonEmpty a b --}

{--
(NonEmpty 1 []) <> (NonEmpty 4 [])
(NonEmpty 1 []) <> (NonEmpty 4 [1, 2])
(NonEmpty 1 [2, 3, 4]) <> (NonEmpty 5 [])
(NonEmpty 1 [2, 3, 4]) <> (NonEmpty 5 [6, 7])
--}

-- 6.4.3 (Medium) Write a functor instance for NonEmpty.
-- 6.4.4 (Difficult) Write a Foldable instance for NonEmpty. Hint: reuse the Foldable instance for arrays.
