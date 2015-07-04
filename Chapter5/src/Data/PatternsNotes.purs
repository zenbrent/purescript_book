module Data.PatternsNotes where

import Data.Foldable
import Data.Maybe
import Data.Picture

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
isEmpty :: forall a. Array a -> Boolean
isEmpty [] = true
isEmpty _ = false


takeFive :: Array Number -> Number
takeFive [0, 1, a, b, _] = a * b
takeFive _ = 0

-- Cons patterns
sumOfSquares :: Array Number -> Number
sumOfSquares [] = 0
sumOfSquares (n : ns) = n * n + sumOfSquares ns
{--
sumOfSquares [0, 1, 2, 3]
--}

sumOfProducts :: Array Number -> Number
sumOfProducts [] = 0
sumOfProducts [_] = 0
sumOfProducts (n : m : ns) = n * m + sumOfProducts (m : ns)


-- 1. (Easy) Write a function allTrue which determines if all elements of an array of Boolean values are equal to true.
allTrue :: Array Boolean -> Boolean
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
isSorted :: forall c. (c -> c -> Boolean) -> Array c -> Boolean
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

totalHeight :: Array Person -> Number
totalHeight [] = 0 
totalHeight ({height = h} : ps) = h + totalHeight ps
{--
totalHeight [{height: 10} ,{height: 11} ,{height: 12} ,{height: 10.5}]
--}

-- 5.9 Named patterns!
dup :: forall a. Array a -> Array a
-- This brings the head element into scope as x
-- and it also binds the entire value as arr
dup arr@(x : _) = x : arr
dup [] = []
{--
dup [1, 2, 3]
--}

-- Exercises 5.9
-- 1. (Easy) Write a function getCity which uses record patterns to find a person's city. A Person should be represented as a record which contains an address field of type Address, and Address should contain the city field.

type Address = {city :: String}
type LocalPerson = {
    first :: String,
    last :: String,
    address :: Address
}
{--
getCity :: LocalPerson -> String
getCity {address: {city = c}} = c
--}
{--
getCity {first: "Brent", last: "Brimhall", address: {city: "Tempe"}}
--}

-- 2. (Medium) What is the most general type of the getCity function, taking into account row polymorphism? What about the totalHeight function defined above?
getCity :: forall r. {address :: Address | r } -> String
getCity {address: {city = c}} = c

-- 3. (Medium) Write a function flatten which uses only patterns and the concatenation (++) operator to flatten an array of arrays into a singly-nested array. Hint: the function should have type forall a. [[a]] -> [a].
flattenArr :: forall a. Array (Array a) -> Array a
flattenArr [[]] = []
flattenArr [[s]] = [s]
flattenArr (s : xs) = s ++ flattenArr xs
{--
flattenArr [[1], [1, 2, 3], [0, 1, 2], [], [1]]
flattenArr [[1]]
flattenArr [[]]
--}

-- 5.10 Case expressions
-- Lets you avoid naming a function just to use a pattern.
lzs :: Array Number -> Array Number
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
{-- This stuff is all defined in Data.Picture
data Shape -- type constructor
    = Circle Point Number -- data constructors
    | Rectangle Point Number Number
    | Line Point Point
    | Text Point String

-- this has a type and data constructor with the same name
-- but they live in different namespaces.
data Point = Point
    { x :: Number
    , y :: Number
    }
--}

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

{-- This stuff is all defined in Data.Picture
showPoint :: Point -> String
showPoint (Point {x = x, y = y}) =
    "(" ++ show x ++ ", " ++ show y ++ ")"
--}

{--
showPoint $ Point {x: 0, y: 0}
showPoint $ Point {x: 10, y: 9}
--}

{-- This stuff is all defined in Data.Picture
showShape :: Shape -> String
showShape (Circle c r) = 
  "Circle [center: " ++ showPoint c ++ ", radius: " ++ show r ++ "]"
showShape (Rectangle c w h) = 
  "Rectangle [center: " ++ showPoint c ++ ", width: " ++ show w ++ ", height: " ++ show h ++ "]"
showShape (Line start end) = 
  "Line [start: " ++ showPoint start ++ ", end: " ++ showPoint end ++ "]"
showShape (Text loc text) = 
  "Text [location: " ++ showPoint loc ++ ", text: " ++ show text ++ "]"
--}

-- Exercises 5.13:
-- 1. (Easy) Construct a value of type Shape which represents a circle centered at the origin with radius 10.
{--
showShape $ Circle (Point {x: 0, y: 0}) 10
--}

-- 2. (Medium) Write a function from Shapes to Shapes, which scales its argument by a factor of 2, center the origin.


-- 3. (Medium) Write a function which extracts the text from a Shape. It should return Maybe String, and use the Nothing constructor if the input is not constructed using Text.
shapeText :: Shape -> Maybe String
shapeText (Text _ str) = Just str
shapeText _ = Nothing

{--
shapeText $ Circle (Point {x: 10, y: 100}) 2
shapeText $ Rectangle (Point {x: 10, y: 100}) 5 15
shapeText $ Line (Point {x: 10, y: 10}) (Point {x: 20, y: 100})
shapeText $ Text (Point {x: 10, y: 10}) "#yolo"
--}

-- 5.14 Newtypes
{-- This stuff is all defined in Data.Picture
newtype Pixels = Pixels Number
newtype Inches = Inches Number
--}
-- It's now impossible to mix up pixels and inches, but there's no runtime perf hit.

-- 5.15 Vector graphics lib!

{--
showPicture [Circle (Point {x: 0, y: 1}) 5, Rectangle (Point {x: 0, y: 1}) 10 2]
--}

-- 5.16 Computing bounding rectangles
{--
emptyBounds
--}

-- 5.16 exercises
-- (Medium) Extend the vector graphics library with a new operation area which computes the area of a Shape. For the purposes of this exercise, the area of a piece of text is assumed to be zero.
shapeArea :: Shape -> Number
shapeArea (Circle _ r) = Math.pi * r * r
shapeArea (Rectangle _ w h) = w * h
shapeArea (Line _ _) = 0
shapeArea (Text _ _) = 0

{--
shapeArea $ Circle (Point {x: 10, y: 100}) 2
shapeArea $ Rectangle (Point {x: 10, y: 100}) 5 15
shapeArea $ Line (Point {x: 10, y: 10}) (Point {x: 20, y: 100})
shapeArea $ Text (Point {x: 10, y: 10}) "#yolo"
--}

-- 2. (Difficult) Extend the Shape type with a new data constructor Clipped, which clips another Picture to a rectangle. Extend the shapeBounds function to compute the bounds of a clipped picture. Note that this makes Shape into a recursive data type.



