module FileOperations where

import Data.Path
import Data.Array

allFiles :: Path -> [Path]
allFiles root = root : concatMap allFiles (ls root)

allFiles' :: Path -> [Path]
allFiles' file = file : do
  child <- ls file
  allFiles' child


-- Some examples of recursion:
fact :: Number -> Number
fact 0 = 1
fact n = n * fact (n-1)

fib :: Number -> Number
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)


recursiveLength :: forall a. [a] ->  Number
recursiveLength arr =
    if Data.Array.null arr
    then 0
    else 1 + length (Data.Array.Unsafe.tail arr)

-- Exercises!
{-- 4.1 Write a recursive function which returns true if and only if its input is an even number. --}
isEven :: Number -> Boolean
isEven 0 = true
isEven 1 = false
isEven n = isEven (n - 2)

{-- 4.2 Write a recursive function which counts the number of even numbers in an array. Hint: use the head function from Data.Array.Unsafe. --}
arrEvenCount :: [Number] -> Number
arrEvenCount arr = arrEvenCount2 0 arr
    where
        arrEvenCount2 :: Number -> [Number] -> Number
        arrEvenCount2 count [] = count
        arrEvenCount2 count arr = 
            if isEven (Data.Array.Unsafe.head arr)
            then arrEvenCount2 (count + 1) (Data.Array.Unsafe.tail arr)
            else arrEvenCount2 count (Data.Array.Unsafe.tail arr)

{--
import Data.Array
map (\n -> n + 1) [1,2,3,4,5,0]

-- Or with infix function application:
(\n -> n + 1) `map` [1,2,3,4,5,0]

-- <$> is equvalent to the map operator.
(\n -> n + 1) <$> [1,2,3,4,5,0]
-- or as a prefix:
(<$>) (\n -> n + 1) [1,2,3,4,5,0]

-- To define a function as infix, put the name in parens, e.g.
-- Data.Array defines this:
(..) :: Number -> Number -> [Number]
(..) = range
-- used as:
arrEvenCount $ 1 .. 9
-- .. has a higer prescedence than <$>
show <$> 1 .. 9
-- defined as such:
infix 5 ..
-- can use infixl or infixr instead to define associativity
--}

{-- 4.7 filtering arrays --}
{--
import Data.Array
filter (\n -> n % 2 == 0) (1 .. 10)
--}

{-- 4.7 exercises: --}
-- Exercise 4.7.1 (Easy) Use the map or <$> function to write a function which calculates the squares of an array of numbers.
sqrAry :: [Number] -> [Number]
sqrAry [] = []
sqrAry ary = (\n -> n * n) <$> ary
{--
import Data.Array
sqrAry []
sqrAry $ 1 .. 10
--}

-- Exercise 4.7.2 (Easy) Use the filter function to write a function which removes the negative numbers from an array of numbers.
{--
removeNegatives :: [Number] -> [Number]
removeNegatives ary = filter (\n -> n >= 0) ary
--}
{--
import Data.Array
removeNegatives $ -9 .. 9
removeNegatives (3 .. -3 ++ 3 .. -3)
--}

-- Exercise 4.7.3 (Medium) Define an infix synonym <$?> for filter. Rewrite your answer to the previous question to use your new operator. Experiment with the precedence level and associativity of your operator in psci.

(<$?>) :: forall a. (a -> Prim.Boolean) -> [a] -> [a]
(<$?>) filterFn arr = filter filterFn arr

removeNegatives :: [Number] -> [Number]
removeNegatives ary = (\n -> n >= 0) <$?> ary

{--
import Data.Array
(\n -> n >= 0) <$?> (3 .. -3 ++ 3 .. -3)

removeNegatives $ -9 .. 9
removeNegatives (3 .. -3 ++ 3 .. -3)
--}

