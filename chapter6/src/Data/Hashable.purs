module Data.Hashable 
  ( HashCode()
  , hashCode

  , Hashable
  , hash
  , hashEqual

  , hasDuplicates
  ) where

import Prelude 

import Data.Maybe
import Data.Tuple
import Data.Either
import Data.String
import Data.Char
import Data.Function (on)
import Data.Foldable (foldMap)
import Data.Monoid (Monoid)

newtype HashCode = HashCode Int

hashCode :: Int -> HashCode
hashCode h = HashCode (h `mod` 65536)

class (Eq a) <= Hashable a where
  hash :: a -> HashCode

instance showHashCode :: Show HashCode where
  show (HashCode h) = "(HashCode " ++ show h ++ ")"

instance eqHashCode :: Eq HashCode where
  eq (HashCode h1) (HashCode h2) = h1 == h2

instance semigroupHashCode :: Semigroup HashCode where
  append (HashCode h1) (HashCode h2) = hashCode (73 * h1 + 51 * h2)

instance monoidHashCode :: Monoid HashCode where
  mempty = hashCode 0

hashEqual :: forall a. (Hashable a) => a -> a -> Boolean
hashEqual = eq `on` hash

instance hashChar :: Hashable Char where
  hash = hash <<< toCharCode

instance hashString :: Hashable String where
  hash = hash <<< toCharArray

instance hashInt :: Hashable Int where
  hash = hashCode

instance hashBoolean :: Hashable Boolean where
  hash false = hashCode 0
  hash true  = hashCode 1

instance hashArray :: (Hashable a) => Hashable (Array a) where
  hash = foldMap hash

instance hashMaybe :: (Hashable a) => Hashable (Maybe a) where
  hash Nothing = hashCode 0
  hash (Just a) = hashCode 1 <> hash a

instance hashTuple :: (Hashable a, Hashable b) => Hashable (Tuple a b) where
  hash (Tuple a b) = hash a <> hash b

instance hashEither :: (Hashable a, Hashable b) => Hashable (Either a b) where
  hash (Left a) = hashCode 0 <> hash a
  hash (Right b) = hashCode 1 <> hash b

-- Exercises
-- 1 (Easy) Use PSCI to test the hash functions for each of the defined instances.
-- ( see ../../tests )

-- 2 (Medium) Use the hashEqual function to write a function which tests if an array has any duplicate elements, using hash-equality as an approximation to value equality. Remember to check for value equality using == if a duplicate pair is found. Hint : the nubBy function in Data.Array should make this task much simpler.

import qualified Data.Array (nubBy, length) as A

hasDuplicates :: forall a. (Hashable a) => Array a -> Boolean
hasDuplicates a = A.length a /= A.length (A.nubBy hashEqual a)

-- 3 (Medium) 
-- Write a Hashable instance for the following newtype which upholds the typeclass law:

{--
newtype Hour = Hour Int

instance eqHour :: Eq Hour where
    eq = eq `on` (`mod` 12)
--}

-- The newtype Hour and its Eq instance represent the type of integers modulo 12, so that 1 and 13 are identified as equal, for example. Prove that the type class law holds for your instance.

-- 4 (Difficult) Prove the type class laws for the Hashable instances for Maybe, Either and Tuple.

