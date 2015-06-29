module Data.PatternsNotes where

import Data.Maybe
import Data.Foldable

-- Euclidean Algorithm with patterns
{--
gcd :: Number -> Number -> Number
gcd n 0 = n
gcd 0 m = m
gcd n m = if n > m then gcd (n - m) m else gcd n (m - n)
--}

{-- 5.4 Different kinds of simple patterns:
Numeric, string, boolean literal
Variable patterns -- bind args to a name
wildcard -- _ matches any arg and doens't bind to a name.
--}

-- 5.5 Guards
gcd :: Number -> Number -> Number
gcd n 0 = n
gcd 0 m = m
gcd n m | n > m = gcd (n - m) m
gcd n m         = gcd n (m - n)

-- 5.5 Exercises:
-- Write factorial fn using patterns.
-- this was already shown in ch 4!
fact :: Number -> Number
fact 0 = 1
fact n = n * fact (n-1)

-- Write Pascal's Rule for computing binomial coefficients.
{--
  (n - 1  + (n - 1  = (n
     k  )    k - 1)    k)
for 1 <= k <= n
where (n  is a binomial coefficient.
       k)
--}

-- 5.6 Array patterns

-- Array literals:
isEmpty :: forall a. [a] -> Boolean
isEmpty [] = true
isEmpty _ = false


takeFive :: [Number] -> Number
takeFive [0, 1, a, b, _] = a * b
takeFive _ = 0

-- Cons patterns
sumOfSquares :: [Number] -> Number
sumOfSquares [] = 0
sumOfSquares (n : ns) = n * n + sumOfSquares ns
{--
sumOfSquares [0, 1, 2, 3]
--}

sumOfProducts :: [Number] -> Number
sumOfProducts [] = 0
sumOfProducts [_] = 0
sumOfProducts (n : m : ns) = n * m + sumOfProducts (m : ns)


-- 1. (Easy) Write a function allTrue which determines if all elements of an array of Boolean values are equal to true.
allTrue :: [Boolean] -> Boolean
allTrue [] = false
allTrue [true] = true
allTrue (n : ns) = n && allTrue ns

{--
allTrue [true, true, true]
allTrue [true, false, true]
allTrue [false, true, true]
allTrue [false, false, false, false]
--}

-- 2. (Medium) Write a function isSorted which tests if an array of numbers is sorted.
isSorted :: forall c. (c -> c -> Boolean) -> [c] -> Boolean
isSorted _ [] = true
isSorted compare [n, m] = compare n m
isSorted compare (n : m : ns) = compare n m && isSorted compare (m : ns)

{--
isSorted (\n m -> n <= m) [1, 0, 2, 3, 4]
import Data.Array
isSorted (\n m -> n <= m) $ 0 .. 1000
--}

-- 5.7 Record patterns
-- Row polymorphism:
showPerson :: forall r. {first :: String, last :: String | r} -> String
showPerson {first = x, last = y} = y ++ ", " ++ x
-- This function is polymorphic in the row r of record fields, hence the name row polymorphism.
{--
showPerson {first: "Brent", last: "Brimhall"}
showPerson {first: "Brent", last: "Brimhall", location: "Tempe"}

let showPerson' { first = x, last = y } = y ++ ", " ++ x
:t showPerson'
showPerson' {first: "Brent", last: "Brimhall", location: "Tempe"}
--}

-- 5.8 Nested patterns
type Person = {height :: Number}

totalHeight :: [Person] -> Number
totalHeight [] = 0 
totalHeight ({height = h} : ps) = h + totalHeight ps
{--
totalHeight [{height: 10} ,{height: 11} ,{height: 12} ,{height: 10.5}]
--}

-- 5.9 Named patterns!
dup :: forall a. [a] -> [a]
-- This brings the head element into scope as x
-- and it also binds the entire value as arr
dup arr@(x : _) = x : arr
dup [] = []
{--
dup [1, 2, 3]
--}

-- Exercises 5.9
-- 1. (Easy)Write a function getCity which uses record patterns to find a person's city. A Person should be represented as a record which contains an address field of type Address, and Address should contain the city field.
-- 2. (Medium) What is the most general type of the getCity function, taking into account row polymorphism? What about the totalHeight function defined above?
-- 3. (Medium) Write a function flatten which uses only patterns and the concatenation (++) operator to flatten an array of arrays into a singly-nested array. Hint: the function should have type forall a. [[a]] -> [a].

-- 5.10 Case expressions
-- Lets you avoid naming a function just to use a pattern.
lzs :: [Number] -> [Number]
lzs [] = []
lzs xs@(_ : t) = case sum xs of
                      0 -> xs
                      _ -> lzs t

{--
lzs [0, 0, 0, 0]
lzs [5, 1, 0, 0, -1]
lzs $ 100000 .. -10
--}

-- 5.11 Pattern match failures
-- It's usually better to make functions "total" rather than "partial"

{--
patternFailure :: Number -> Number
patternFailure 0 = 0

patternFailure 0
patternFailure 1
--}

-- better to do this:
patternFailure :: Number -> Maybe Number
patternFailure 0 = Just 0
patternFailure _ = Nothing

-- 5.12 ADT
data Shape -- type constructor
    = Circle Point Number -- data constructors
    | Rectangle Point Number
    | Line Point Point
    | Text Point String

-- this has a type and data constructor with the same name
-- but they live in different namespaces.
data Point = Point
    { x :: Number
    , y :: Number
    }

{-- Maybe is defined as thus:
data Maybe a = Nothing | Just a
--}

{-- Data constructors can also define recursive types:
data List a = Nil | Cons a (List a)
--}

exampleLine :: Shape
exampleLine = Line origin origin
  where
      origin :: Point
      origin = Point {x: 0, y: 0}

-- 5.13 Using ADTs
-- The only way to consume a value of an ADT is to use a pattern
-- to match its constructor!

showPoint :: Point -> String
showPoint (Point {x = x, y = y}) = "(" ++ show x ++ ", " ++ show y ++ ")"

{--
showShape :: Shape -> String
showShape (Circle c r) = ...
showShape (Rectangle c w h) = ...
showShape (Line start end) = ...
showShape (Circle loc text) = ...
--}

{--
showPoint $ Point {x: 0, y: 0}
showPoint $ Point {x: 10, y: 9}
--}


-- Exercises 5.13:
-- 1. (Easy) Construct a valueoftypeShapewhichrepresentsacirclecenteredattheoriginwith radius 10.
-- 2. (Medium)WriteafunctionfromShapestoShapes,whichscalesitsargumentbyafactorof 2, center the origin.
-- 3. (Medium) Write a function which extracts the text from a Shape. It should return Maybe String, and use the Nothing constructor if the input is not constructed using Text.

-- 5.14 Newtypes
newtype Pixels = Pixels Number
newtype Inches = Inches Number
-- It's now impossible to mix up pixels and inches, but there's no runtime perf hit.
