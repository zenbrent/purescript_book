module Notes (
    Extended(..),
    Action, act
    ) where

import Prelude 

import Data.Maybe
import Data.Tuple
import Data.Either
import Data.String
import Data.Char
import Data.Function (on)
import Data.Foldable
import Data.Monoid (Monoid)

{--
class Show a where
    show :: a -> String

instance showBoolean :: Show Boolean where
    show true = "true"
    show false = "false"
--}

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

{--
import Data.Either
show $ Left 10.0
show $ (Left 10.0 :: Either Number String)
--}

{--
class Semigroup a where
    append :: a -> a -> a

-- both (++) and (<>) are aliases for append

-- Monoids describe how to accumulate results with that type, starting with an empty value and combining new results.
class (Semigroup m) <= Monoid m where
    mempty :: m

class Foldable f where
    foldl :: forall a b. (b -> a -> b) -> b -> f a -> b
    foldr :: forall a b. (a -> b -> b) -> b -> f a -> b
    foldMap :: forall a m. (Monoid m) => (a -> m) -> f a -> m
--}

-- Functors!
{--
class Functor f where
    map :: forall a b. (a -> b) f a -> f b
-- (<$>) is a alias for map.

-- functor laws:
-- identity law:
id <$> xs = xs
-- composition law:
g <$> (f <$> xs) = (g <<< f) <$> xs
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
show (Complex {real: 10.0, imaginary: 3.0})
--}

instance eqComplex :: Eq Complex where
    eq (Complex a) (Complex b) = a.real == b.real && a.imaginary == b.imaginary

{-- true false false true:
(Complex {real: 10.0, imaginary: 3.0}) == (Complex {real: 10.0, imaginary: 3.0})
(Complex {real: 10.0, imaginary: 3.0}) /= (Complex {real: 10.0, imaginary: 3.0})
(Complex {real: 10.0, imaginary: 4.0}) == (Complex {real: 10.0, imaginary: 3.0})
(Complex {real: 10.0, imaginary: 3.0}) /= (Complex {real: 1.0, imaginary: 3.0})
--}

-- 6.4.2 (Medium) The following type defines a type of non-empty arrays of elements of type a:

data NonEmpty a = NonEmpty a (Array a)

-- Write a Semigroup instance for non-empty arrays by reusing the Semigroup instance for Array a.

instance semigroupNonEmpty :: Semigroup (NonEmpty a) where
    append (NonEmpty a b) (NonEmpty c d) = NonEmpty a (b <> [c] <> d)

instance showNonEmpty :: Show (NonEmpty Number) where
    show (NonEmpty a b) = show $ [a] ++ b

concatNonEmpty :: forall a. (NonEmpty a) -> (NonEmpty a) -> (NonEmpty a)
concatNonEmpty (NonEmpty a b) (NonEmpty c d) = NonEmpty a b

{--
(NonEmpty 1.0 []) <> (NonEmpty 4.0 [])
(NonEmpty 1.0 []) <> (NonEmpty 4.0 [1.0, 2.0])
(NonEmpty 1.0 [2.0, 3.0, 4.0]) <> (NonEmpty 5.0 [])
(NonEmpty 1.0 [2.0, 3.0, 4.0]) <> (NonEmpty 5.0 [6.0, 7.0])
--}

-- 6.4.3 (Medium) Write a functor instance for NonEmpty.

instance functorNonEmpty :: Functor NonEmpty where
    map fn (NonEmpty a b) = NonEmpty (fn a) (fn <$> b)

{--
(\a -> a * 2) <$> NonEmpty 9 []
(\a -> a * 2) <$> NonEmpty (-1) [1, 0, 2, 3]
--}

-- 6.4.4 (Difficult) Write a Foldable instance for NonEmpty. Hint: reuse the Foldable instance for arrays.

-- This can be specialized where f is Array, so you can replace f a with Array a
import Data.Foldable
import Data.Monoid

-- Monoid describes the result of a fold, Foldable describes the type constructors of things that can be the used as a source for a fold.
-- Foldable abstracts the concept of an ordered container.

instance foldableNonEmpty :: Foldable NonEmpty where
    -- foldl :: forall a b. (a -> b -> a) -> b -> f a -> b
    foldl fn acc (NonEmpty a b) = foldl fn acc ([a] <> b)

    -- foldr :: forall a b. (b -> a -> b) -> b -> f a -> b
    foldr fn acc (NonEmpty a b) = foldr fn acc ([a] <> b)

    -- foldMap :: forall a m. (Monoid m)  => (a -> m) -> f a -> m
    foldMap fn (NonEmpty a b) = foldMap fn ([a] <> b)

{--
foldl (<>) mempty (NonEmpty "Hello" [" ", "World"])
foldl (<>) mempty (NonEmpty [1, 2, 3] [[4], [5, 6]])
foldr (<>) mempty (NonEmpty "Hello" [" ", "World"])
foldMap ("" <>) (NonEmpty "Hello" [" ", "World"])
--}


-- 6.5 Type Annotations
-- Types of functions can be constrained using type classes.
threeAreEqual :: forall a. (Eq a) => a -> a -> a -> Boolean
threeAreEqual a1 a2 a3 = a1 == a2 && a2 == a3
-- a can be any type, as long as there's an Eq instance for a.
-- can csv them: ... :: forall a. (Eq a, Show a) => ...

{-- Psc can't infer a constrained type.
:type \x -> x + x
:type \x -> x + (x :: Number)
--}

-- 6.7 Instance deps
{--
instance showArray :: (Show a) => Show (Array a) where
    ...
--}


-- 6.7 Exercises!

-- 6.7.1 (easy) Write an Eq instance for the type NonEmpty a, which reuses the instances for Eq a and Eq (Array a)

instance eqNonEmpty :: (Eq a) => Eq (NonEmpty a) where
    eq (NonEmpty e1 a1) (NonEmpty e2 a2) = e1 == e2 && a1 == a2

{--
eq (NonEmpty 1 [1, 2, 3]) (NonEmpty 1 [1, 2, 3])
eq (NonEmpty 2 [1, 2, 3]) (NonEmpty 1 [1, 2, 3])
(NonEmpty 1 [1, 2, 3]) == (NonEmpty 1 [1, 3, 3])
eq (NonEmpty 1 []) (NonEmpty 1 [1, 3, 3])
(NonEmpty 1 []) == (NonEmpty 1 [])
--}

-- 6.7.2 (Medium) Given any type a with an instance of Ord, we can add anew "infinite" value which is greater than any other value:

data Extended a = Finite a | Infinite

-- Write an Ord instance for Extended a which reuses the Ord instance for a.

instance eqExtended :: (Eq a) => Eq (Extended a) where
    eq Infinite Infinite = true
    eq (Finite a) (Finite b) = eq a b
    eq _ _ = false

instance ordExtended :: (Ord a) => Ord (Extended a) where
    compare Infinite Infinite = EQ
    compare (Finite _) Infinite = LT
    compare Infinite (Finite _) = GT
    compare (Finite a) (Finite b) = compare a b

{--
(Finite 5) > Infinite
(Finite 5) > (Finite 4)
Infinite > (Finite 5)
Infinite == (Finite 5)
(Finite 5) == (Finite 5)
(Finite 5) == (Finite 3)
--}

-- 6.9 Nullary Type Classes

{--
module Partial where

class Partial

head :: forall a. (Partial) => Array a -> a
head = Data.Array.Unsafe.head

tail :: forall a. (Partial) => Array a -> Array a
tail = Data.Array.Unsafe.tail

-- Note that we do not define an instance for the Partial type class in the Partial module. Doing so would defeat its purpose: with this definition, attempting to use the head function will result in a type error:
{ --
Partial.head [1,2,3]
-- }
-- The user of this lib has 2 options:

-- Opt into partiality by declaring an instance of Partial
module Main where

import Partial
instance partial :: Partial

-- or republish the constraint:
secondElement :: forall a. (Partial) => Array a -> a
secondElement xs = head (tail xs)
--}

-- 6.10 Typeclasses
-- In general, it makes sense to define a superclass relationship when the laws for the subclass mention the members of the superclass.

-- 6.10 Exercises

-- 1. (Medium) The Action class is a multi-parameter type class which defines an action of one type on another:

class (Monoid m) <= Action m a where
    act :: m -> a -> a

-- An action is a function which describes how a monoid can be used to modify a value of another type. We expect the action to respect the concatenation operator of the monoid. For example, the monoid of natural numbers with multiplication acts on strings by repeating a string some number of times:

instance semigroupInt :: Semigroup Int where
    append a b = a + b

instance monoidInt :: Monoid Int where
    mempty = 0

instance repeatAction :: Action Int String where
    act 0 _ = ""
    act n s = s ++ act (n - 1) s


-- Note that act 2 s is equal to the combination act 1 s <> act 1 s, and 1 <> 1 = 2 in the monoid of additive integers.
-- Write down a reasonable set of laws which describe how the Action class should interact with the Monoid class. Hint: how do we expect mempty to act on elements? What about append?

{--
act 1 "s" <> act 1 "s" == act 2 "s"
--}

-- 6.10.2 (Medium) Write an instance Action m a => Action m (Array a) where the action on arrays is defined by acting on the elements independently.

import Data.Array.Unsafe (head, tail)
-- import Data.Array (tail)

instance arrayAction :: (Monoid m, Action m a) => Action m (Array a) where
    act _ [] = []
    act m x = [(act m (head x))] <> maybeAct m (tail x) where
        maybeAct :: m -> (Array a) -> Array
        maybeAct m [] = []
        maybeAct m x = act m x

{--
act 5 ["a", "b", "c"]
act 0 ["a", "b", "c"]
act 0 ([] :: Array String)
act 10 ([] :: Array String)
--}

-- 6.10.3 (Difficult) TODO
-- 6.10.4 (Medium) TODO



