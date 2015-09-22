module Exercises where

import Prelude

-- import DOM

import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Eff.Random

import Data.Array
-- import qualified Data.Array as A
-- import Data.List
import Data.Maybe
-- import Data.AddressBook.UI


-- Exercise 8.7.1 (Easy) Look up the types of the head and tail functions from the Data.Array module in the purescript-arrays package. Use do notation with the Maybe monad to combine these functions into a function third which returns the third element of an array with three or more elements. Your function should return an appropriate Maybe type.

{--
third :: forall a. Array a -> Maybe a
third arr = do
  a <- A.tail arr
  b <- A.tail a
  c <- A.head b
  return c
  --}

-- Exercise 8.7.2 (Medium) Write a function sums which uses foldM to determine all possible totals that could be made using a set of coins. The coins will be specified as an array which contains the value of each coin. Your function should have the following result:

{--
> sums []
[0]

> sums [1, 2, 10]
[0,1,2,3,10,11,12,13]
--}


-- sums :: (Array Int) -> Array (Array Int)
-- sums coins = foldM (\a b -> [[0, a] ++ b]) coins ([] :: Array (Array Int))

-- via https://www.snip2code.com/Snippet/658139/Solution-to-exercise-2-in-section-8-7-in/ :

sums :: Array Int -> Array (Array Int)
sums = foldM addAndSortShit [0] where
  addAndSortShit a e = [(e :) $ map (+e) a ++ a]
  -- addAndSortShit a e = [sort $ nub $ (e :) $ map (+e) a ++ a]

-- Hint: This function can be written as a one-liner using foldM. You might want to use the nub and sort functions to remove duplicates and sort the result respectively.

-- Exercise 8.7.3 (Medium) Confirm that the ap function and the apply operator agree for the Maybe monad.

-- Exercise 8.7.4 (Medium) Verify that the monad laws hold for the Monad instance for the Maybe type, as defined in the purescript-maybe package.

-- Exercise 8.7.5 (Medium) Write a function filterM which generalizes the filter function on lists. Your function should have the following type signature:
{--
filterM :: forall m a. (Monad m) => (a -> m Boolean) -> List a -> m (List a)
--}
-- Test your function in PSCi using the Maybe and Array monads.
-- Exercise 8.7.6 (Difficult) Every monad has a default Functor instance given by:
{--
map f a = do
  x <- a
  return (f a)
--}
-- Use the monad laws to prove that for any monad, the following holds:
{--
lift2 f (return a) (return b) = return (f a b)
--}
-- where the Applicative instance uses the ap function defined above. Recall that lift2 was defined as follows:
{--
lift2 :: forall f a b c. (Applicative f). (a -> b -> c) -> f a -> f b -> f c
lift2 f a b = f <$> a <*> b
--}


-- 8.10 The Eff Monad

doRandom :: Eff (random :: RANDOM, console :: CONSOLE) Unit
doRandom = do
  n <- random
  print n

