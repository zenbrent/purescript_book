module Exercises.Monad where

import Prelude

import Data.Foldable (traverse_)
import Control.Monad.State
import Control.Monad.State.Class

sumArray :: Array Number -> State Number Unit
sumArray = traverse_ $ \n -> modify (\sum -> sum + n)

{--
runState (do
    sumArray [1.0, 2.0]
    sumArray [4.0, 4.0]
    sumArray [1.2, 4.5, 1.0, 2.0]
    ) 0.0

--}

{-- Exercise 11.4.2
-- (Medium) A string of parentheses is balanced if it is obtained by either concatenating zero-or-more shorter balanced strings, or by wrapping a shorter balanced string in a pair of parentheses.
Use the State monad and the traverse_ function to write a function

 testParens :: String -> Boolean
which tests whether or not a String of parentheses is balanced, by keeping track of the number of opening parentheses which have not been closed. Your function should work as follows:

 > testParens ""
 true
    
 > testParens "(()(())())"
 true
    
 > testParens ")"
 false
    
 > testParens "(()()"
 false
Hint: you may like to use the split function from the Data.String module to turn the input string into an array of characters.
--}


