module ValidatingAddresses where

import Prelude


import qualified Data.Maybe as M
import Data.List
import Data.Either
import Data.Validation
import Data.AddressBook
import Data.Traversable

import qualified Data.String as S
import qualified Data.String.Regex as R

import Control.Apply

import Data.AddressBook
import Data.AddressBook.Validation

{--
validateAddress $ address "" "" ""
validateAddress $ address "1234 N. South Street" "Tempe" "AZ"
--}

-- 1. (Easy) Use a regular expression validator to ensure that the state field of the Address type contains two alphabetic characters. Hint: see the source code for phoneNumberRegex.
-- done, see Data.AddressBook.Validation

-- 2. (Medium) Using the matches validator, write a validation function which checks that a string is not entirely whitespace. Use it to replace nonEmpty where appropriate.

-- 7.11 Traversable Functors

-- TODO:
-- Exercise for the reader, p89:
-- Implement sequence and traverse in terms of each other.

-- 7.11.1
-- 1. (Medium) Write a Traversable instance for the following binary tree data structure, which combines side-effects from left-to-right:

data Tree a = Leaf | Branch (Tree a) a (Tree a)

{--
instance traversableTree :: Traversable Tree a where
    traverse f Leaf = 
--}

-- This corresponds to an in-order traversal of the tree. What about a preorder traversal? What about reverse order?



-- instance traverseTree :: 

-- 7.11.2 (Medium) Modify the code to make the address field of the Person type optional using Data.Maybe. Hint : Use traverse to validate a field of type Maybe a.

-- 3. (Difficult) Try to write sequence in terms of traverse. Can you write traverse interms ofsequence?
