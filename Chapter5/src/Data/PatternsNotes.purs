module Data.PatternsNotes where

import Data.Foldable

-- Euclidean Algorithm with patterns
gcd :: Number -> Number -> Number
gcd n 0 = n
gcd 0 m = m
gcd n m = if n > m then gcd (n - m) m else gcd n (m - n)


{-- 5.4 Different kinds of simple patterns:
Numeric, string, boolean literal
Variable patterns -- bind args to a name
wildcard -- _ matches any arg and doens't bind to a name.
--}




