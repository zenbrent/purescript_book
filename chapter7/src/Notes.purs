module Notes where

import Prelude

import qualified Data.Maybe as M
import Data.List
import Data.Either
import Data.Validation
import Data.AddressBook
import Data.Traversable

import qualified Data.String as S
import qualified Data.String.Regex as R

import Control.Apply

import Data.AddressBook

{--
lift3 address (Just "123 some street") (Just "Tempe") (Just "AZ")
lift3 address (Just "123 some street") Nothing (Just "AZ")
--}

{--
class Functor f where
    map :: forall a b. (a -> b) -> f a -> f b
    -- aliased to <$>

class (Functor f) <= Apply f where
    apply :: forall a b. f (a -> b) -> f a -> f b
    -- aliased to <*>


instance functorMaybe :: Functor Maybe where
    map f (Just a) = Just (f a)
    map f Nothing = Nothing

-- We can apply an optional function to an optional value, and the result is defined iff they are both defined.
instance applyMaybe :: Apply Maybe where
    apply (Just f) (Just x) = Just (f x)
    apply _ _ = Nothing
--}

{--
lift3 :: forall a b c d f. (Prelude.Apply f) =>
                           (a -> b -> c -> d) ->
                           f a -> f b -> f c -> f d
lift3 f x y z = f <$> x <*> y <*> z
--}

{--
address <$> Just "123 some street" <*> Just "Tempe" <*> Just "AZ"
address <$> Just "123 some street" <*> Nothing <*> Just "AZ"
address <$> Nothing <*> Nothing <*> Nothing
--}

{--
-- Take a value and return a value wrapped in type constructor f.
class (Apply f) <= Applicative f where
    pure :: forall a -> f a

-- Applicative functors allow lifting of functions. pure lifts functions of 0 arguments.
-- They allow us to work in larger "languages" that support side effects, encoded by the functor f.
--}

-- Convert optional arguments into computations which can signal an error using Either String
{--
let (<?>) Nothing err = Left err
    (<?>) (Just a) _ = Right a

let fullName first middle last = last ++ ", " ++ first ++ " " ++ middle

let fullNameEither first middle last =
    fullName <$> (first <?> "First name was missing")
             <*> (middle <?> "Middle name was missing")
             <*> (last <?> "Last name was missing")

:t fullNameEither
-- Maybe String -> Maybe String -> Maybe String -> Either String String

fullName <$> Just "Brent" <*> Just "Matthew" <*> Just "Brimhall"
fullName <$> Nothing <*> Just "Matthew" <*> Just "Brimhall"
fullNameEither (Just "Brent") (Just "Matthew") (Just "Brimhall")
fullNameEither Nothing Nothing Nothing
--}

-- 7.8 Combining Effects

combineList :: forall f a. (Applicative f) => List (f a) -> f (List a)
combineList Nil = pure Nil
combineList (Cons x xs) = Cons <$> x <*> combineList xs

{--
combineList (toList [Just 1, Just 2, Just 3])
combineList (toList [Just 1, Nothing, Just 3])
--}


-- Exercises 7.8!
-- 7.8.1 (Easy) Use lift2 to write lifted versions of the numeric operators +, -, * and / which work with optional arguments.

instance maybeSemiring :: (Semiring a) => Semiring (M.Maybe a) where
    add = lift2 add
    mul = lift2 mul
    zero = M.Just zero
    one = M.Just one

{--
(M.Just 2) + (M.Just 3)
(+) M.Nothing (M.Just 1)
--}

-- 7.8.2 (Medium) Convince yourself that the definition of lift3 given above in terms of <$> and <*> does type check.

-- 7.8.3 (Difficult) Write a function combineMaybe which has type
-- forall a f. (Applicative f)=> Maybe (f a) -> f (Maybe a)
-- This function takes an optional computation with side-effects, and returns a side-effecting computation which has an optional result.




