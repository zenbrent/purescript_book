module Exercises.Parens where

import Prelude

import Data.Foldable (traverse_)
import Control.Monad.State
import Control.Monad.State.Class
import Data.String
import Data.Maybe

sumArray :: Array Number -> State Number Unit
sumArray = traverse_ $ \n -> modify (\sum -> sum + n)

{--
execState (do
    sumArray [1.0,2.0,3.0]) 0.0
--}

data Paren = L | R

instance showParen :: Show Paren where
    show L = "("
    show R = ")"

parenCount :: Paren -> Int
parenCount L = 1
parenCount R = -1

sumParens :: Array Paren -> State Int Unit
sumParens = traverse_ $ \n -> modify (\sum -> sum + parenCount n)

parseParens :: String -> Array Paren
parseParens s = do
    p <- (split "" s)
    case p of
         "(" -> return L
         ")" -> return R

testParens :: String -> Boolean
testParens s = 0 == execState (do
    sumParens $ parseParens s) 0

{--
testParens ""
testParens "(()(())())"
testParens ")"
testParens "(()()"
--}
