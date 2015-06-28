module FileOperations where

import Control.MonadPlus
import Data.Array
import Data.Foldable
import Data.Path
import Data.Maybe

{--
allFiles :: Path -> [Path]
allFiles root = root : concatMap allFiles (ls root)

allFiles' :: Path -> [Path]
allFiles' file = file : do
  child <- ls file
  allFiles' child
--}


-- Some examples of recursion:
{--
fact :: Number -> Number
fact 0 = 1
fact n = n * fact (n-1)
--}

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

-- 4.8 Flattening arrays
{--
concat [[1, 2], [3, 4]]
concatMap (\n -> [n, n * n]) [1, 2, 3]
--}

{-- 4.9 Array Comprehensions
-- find all the factors of a number.
import Data.Foldable
let pairs n = concatMap (\i -> map (\j -> [i, j]) (i .. n)) (1 .. n)
let factors n = filter (\pair -> product pair == n) (pairs n)
--}

{-- 4.10 Do notation!
-- Just like map and concatMap allow array comprehensions, <$> and >>= allow monad comprehensions.
factors :: Number -> [[Number]]
factors n = filter (\xs -> product xs == n) $ do
    -- bind elements of the array to a name --  <- can be read as "choose"
    i <- 1 .. n
    j <- i .. n
    -- doesn't bind anything
    return [i, j]
    -- instead of return, we could do
    -- [[i, j]]
    -- can also give names to expressions, using let
--}

{-- 4.11 Guards
--}
factors :: Number -> [[Number]]
factors n = do
    i <- range 1 n
    j <- range i n
    guard $ i * j == n
    return [i, j]


-- 1. (Easy) Use the factors function to define a function isPrime which tests if its integer argument is prime or not.
isPrime :: Number -> Boolean
isPrime n = (length $ factors n) == 1
{--
isPrime 8
isPrime 7
isPrime 1008
isPrime 1009
--}

-- 2. (Medium) Write function which uses do notation to find the cartesian product of two arrays, i.e. the set of all tuples of elements a, b, where a is an element of the first array, and b is an element of the second.
cartesianProd :: forall a. [a] -> [a] -> [[a]]
cartesianProd first second = do
    i <- first
    j <- second
    return [i, j]
{--
cartesianProd [1, 2, 3] [4, 5, 6]
cartesianProd [1, 2] [4, 5, 6]
cartesianProd ["1", "2", "3"] ["4", "5"]
--}

-- 3. (Medium) A Pythagorean triple is an array of numbers [a,b,c] such that a^2 + b^2 = c^2. Use the guard function in an array comprehension to write a function triples which takes a number n and calculates all Pythagorean triples whose components are less than n. Your function should have type Number -> [[Number]].
triples :: Number -> [[Number]]
triples 1 = []
triples total = do
    a <- range 1 $ total - 1
    b <- range a $ total - 1
    c <- range 1 $ total
    guard (Math.pow a 2 + Math.pow b 2 == Math.pow c 2)
    return [a, b, c]

primitiveTriples :: Number -> [[Number]]
primitiveTriples 1 = []
primitiveTriples total = do
    a <- range 1 $ total - 1
    b <- range a $ total - 1
    guard (isPrime $ a + b) -- if you want primitive triples - this is crazy slow.
    c <- range 1 $ total
    guard (Math.pow a 2 + Math.pow b 2 == Math.pow c 2)
    return [a, b, c]
{--
-- Via wikipedia, all primitive (a and b are coprime) triples < 100:
-- (3, 4, 5) (5, 12, 13) (8, 15, 17) (7, 24, 25)
-- (20, 21, 29) (12, 35, 37) (9, 40, 41) (28, 45, 53)
-- (11, 60, 61) (16, 63, 65) (33, 56, 65) (48, 55, 73)
-- (13, 84, 85) (36, 77, 85) (39, 80, 89) (65, 72, 97)
import FileOperations
import Data.Array
length $ primitiveTriples 25
primitiveTriples 25
--}

-- 4. (Difficult) Look up the any function from Data.Foldable. Rewrite the factors function to use the any function instead of an array comprehension. Note: the type of any as reported by psci is more general than you need. For the purposes of this exercise, you can assume the type of any is forall a. (a -> Boolean) -> [a] -> Boolean.
{-- --}

-- 5. (Diabolical) Use the factors function to define a function factorizations which produces all factorizations of a number n, i.e. arrays of integers whose product is n. Hint : consider the factorizations of 1 separately. Be careful to avoid infinite recursion.
{-- --}


-- 4.12 Folds!

import Data.Foldable
-- the type of foldl can be specified to:
-- forall a b. (b -> a -> b) -> b -> [a] -> b
-- and foldr:
-- forall a b. (a -> b -> b) -> b -> [a] -> b

{--
foldl (+) 0 (1 .. 5)
foldl (\acc n -> acc ++ show n) "" [1,2,3,4,5]
-- this is equivalent to
((((("" ++ show 1) ++ show 2) ++ show 3) ++ show 4) ++ show 5)

foldr (\n acc -> acc ++ show n) "" [1,2,3,4,5]
((((("" ++ show 5) ++ show 4) ++ show 3) ++ show 2) ++ show 1)
--}

-- 4.13 Tail Recursion
{--
let f 0 = 0
    f n = 1 + f (n - 1)

fact :: Number -> Number -> Number
fact 0 acc = acc
fact n acc = fact (n - 1) (acc * n)
--}

-- 4.14 Accumulators
{--

-- Not tail call recursive:
reverseArr :: forall a. [a] -> [a]
reverseArr [] = []
reverseArr (x : xs) = reverseArr xs ++ [x]

-- Tail call recursive:
reverseArr :: forall a. [a] -> [a]
reverseArr = reverseArr' []
  where
      reverseArr' acc [] = acc
      reverseArr' acc (x : xs) = reverseArr' (x : acc) xs
--}

-- 4.15 Prefer folds to explicit recursion!
-- e.g.
{--
reverseArr :: forall a. [a] -> [a]
reverseArr = foldr (\x xs -> xs ++ [x]) []
--}

-- Exercises 4.14
-- 1. (Easy) Use foldl to test whether an array of boolean values are all true.
areAllTrue :: [Boolean] -> Boolean
areAllTrue [] = false
areAllTrue arr = foldl (\x xs -> xs && x) true arr
{--
areAllTrue [true, false, true, true]
areAllTrue [true, true, true]
areAllTrue [true]
areAllTrue [false]
areAllTrue []
--}

-- 2. (Medium) Characterize those arrays xs for which the function `foldl (==) false xs` returns true.
{--
foldl (==) false xs
--}
{-- 3. (Medium) Rewrite the following function in tail recursive form using an accumulator parameter:
count :: forall a. (a -> Boolean) -> [a] -> Number
count _ [] = 0
count p (x : xs) = if p x then 1 + count p xs else count p xs
--}

count :: forall a. (a -> Boolean) -> [a] -> Number
count p arr = count' 0 p arr
  where
      count' :: forall a. Number -> (a -> Boolean) -> [a] -> Number
      count' acc _ [] = acc
      count' acc p (x : xs) =
          if p x
             then count' (acc + 1) p xs
             else count' acc p xs

{--
count (\x -> x < 3) [1, 2, 3, 4]
count (\x -> x > 3) 1 .. 100000
--}

-- 4. (Medium) Write reverse in terms of foldl.
reverseArr :: forall a. [a] -> [a]
reverseArr = foldl (\x xs -> [xs] ++ x) []
-- reverseArr = foldr (\x xs -> xs ++ [x]) []
{--
reverseArr [1, 2, 3, 4]
--}

-- 4.16 A Virtual Filesystem

-- Listing all files:
allFiles :: Path -> [Path]
-- not tco recursive:
-- allFiles file = file : concatMap allFiles (ls file)
allFiles file = file : do
    child <- ls file
    allFiles child


-- 1. (Easy) Write a function onlyFiles which returns all files (not directories) in all subdirectories of a directory.
onlyFiles :: Path -> [Path]
-- onlyFiles file = filter (\d -> not $ isDirectory d) (allFiles file)
onlyFiles file = filter (not <<< isDirectory) (allFiles file)

{--
onlyFiles root
--}

-- 2. (Medium) Write a fold to determine the largest and smallest files in the filesystem.

{-- 3. (Difficult) Write a function whereIs to search for a file by name. The function should return a value of type Maybe Path, indicating the directory containing the file, if it exists. It should behave as follows:
> whereIs "/bin/ls"
Just (/bin/)

> whereIs "/bin/cat"
Nothing
-- Hint: Try to write this function as an array comprehension using do notation.
--}
