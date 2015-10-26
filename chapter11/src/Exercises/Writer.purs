module Exercises.Writer where

import Prelude

import Control.Monad.Writer
import Control.Monad.Writer.Class

gcd :: Int -> Int -> Writer (Array String) Int
gcd n 0 = return n
gcd 0 m = return m
gcd n m = do
    tell ["gcd " ++ show n ++ " " ++ show m]
    if n > m
       then gcd (n - m) m
       else gcd n (m - n)

{--
runWriter (gcd 21 15)
--}



