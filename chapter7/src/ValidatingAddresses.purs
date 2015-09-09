module ValidatingAddresses
    ( Tree(..)
    ) where

import Prelude

import qualified Data.Maybe as M
import Data.Monoid
import Data.List
import Data.Either
import Data.Validation
import Data.AddressBook
import Data.Traversable
import Data.Foldable

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

instance showTree :: (Show a) => Show (Tree a) where
    show Leaf = "L"
    show (Branch a b c) = "(Branch " ++ show a ++ " . " ++ show b ++ " . " ++ show c ++ ")"

{--
show (Branch Leaf "a node" Leaf)
show (Branch (Branch Leaf "NODES" Leaf) "a node" (Branch Leaf "another node" Leaf))
show (Branch (Branch Leaf "NODES" (Branch Leaf "a node" Leaf)) "a node" (Branch Leaf "another node" Leaf))
--}

instance functorTree :: Functor Tree where
    map f (Branch a b c) = Branch (f <$> a) (f b) (f <$> c)
    map _ Leaf = Leaf

{--
map (\x -> x * x) (Branch (Branch Leaf 5 (Branch Leaf 2 Leaf)) 3 (Branch Leaf 4 Leaf))
--}

-- The type of apply looks a lot like the type of map. The difference between map and apply is that map takes a function as an argument, whereas the first argument to apply is wrapped in the type constructor f.
instance applyTree :: Apply Tree where
    apply (Branch _ f _) a = f <$> a
    apply Leaf _ = Leaf

instance applicativeTree :: Applicative Tree where
    pure x = Branch Leaf x Leaf

{--
(pure id) <*> (Branch Leaf "asdf" Leaf)
(pure (\x -> x + 1)) <*> (Branch Leaf 2 Leaf)
--}

instance semigroupTree :: (Semigroup a) => Semigroup (Tree a) where
    append Leaf Leaf = Leaf
    append x Leaf = x
    append Leaf x = x
    append (Branch l x r) b = Branch (append l b) x r

{--
(Branch Leaf 1 Leaf) <> (Branch Leaf 2 Leaf)
(Branch (Branch Leaf 3 Leaf) 1 Leaf) <> (Branch Leaf 2 Leaf)
(Branch Leaf 1 (Branch Leaf 3 Leaf)) <> (Branch Leaf 2 Leaf)
(Branch (Branch Leaf 3 Leaf) 1 (Branch Leaf 4 Leaf)) <> (Branch Leaf 2 Leaf)

-- associative for this trivial example:
((Branch Leaf 1 Leaf) <> (Branch Leaf 2 Leaf)) <> (Branch Leaf 2 Leaf)
(Branch Leaf 1 Leaf) <> ((Branch Leaf 2 Leaf) <> (Branch Leaf 2 Leaf))
--}

{--
instance monoidTree :: (Monoid a) => Monoid (Tree a) where
    mempty = Leaf
--}

instance foldableTree :: Foldable Tree where
  -- foldl :: forall a b. (b -> a -> b) -> b -> Tree a -> b
  foldl f acc Leaf = acc
  foldl f acc (Branch l x r) = (foldl f (f (foldl f acc l) x) r)
  -- foldr :: forall a b. (a -> b -> b) -> b -> Tree a -> b
  foldr f acc Leaf = acc
  foldr f acc (Branch l x r) = (foldr f (f x (foldr f acc r)) l)
  -- foldMap :: forall a m. (Monoid m) => (a -> m) -> Tree a -> m
  foldMap f xs = foldr (\x acc -> f x <> acc) mempty xs

{--
foldl (+) 0 (Branch Leaf 1 Leaf)
foldl (-) 0 [3, 1, 4]
foldl (-) 0 (Branch (Branch Leaf 3 Leaf) 1 (Branch Leaf 4 Leaf))
foldr (-) 0 [3, 1, 4]
foldr (-) 0 (Branch (Branch Leaf 3 Leaf) 1 (Branch Leaf 4 Leaf))

-- associative for this trivial example:
((Branch Leaf 1 Leaf) <> (Branch Leaf 2 Leaf)) <> (Branch Leaf 2 Leaf)
(Branch Leaf 1 Leaf) <> ((Branch Leaf 2 Leaf) <> (Branch Leaf 2 Leaf))
--}

-- wtf??? Pretty sure this is wrong.
instance traversableTree :: Traversable Tree where
    -- traverse :: forall a b m. (Applicative m) => (a -> m b) -> Tree a -> m (Tree b)
    traverse f (Branch a b c) = Branch <$> traverse f a <*> f b <*> traverse f c
    traverse _ Leaf = pure Leaf

    -- sequence :: forall a m. (Applicative m) => Tree (m a) -> m (Tree a)
    sequence (Branch a b c) = Branch <$> sequence a <*> b <*> sequence c
    sequence Leaf = pure Leaf

{--
traverse (\n -> M.Just n) (Branch Leaf 1 Leaf)
traverse (\n -> M.Just n) (Branch (Branch Leaf 3 Leaf) 1 (Branch Leaf 4 Leaf))
show (Branch <$> Leaf <*> (Branch Leaf 1 Leaf) <*> Leaf)
--}

-- This corresponds to an in-order traversal of the tree. What about a preorder traversal? What about reverse order?



-- instance traverseTree :: 

-- 7.11.2 (Medium) Modify the code to make the address field of the Person type optional using Data.Maybe. Hint : Use traverse to validate a field of type Maybe a.

-- 3. (Difficult) Try to write sequence in terms of traverse. Can you write traverse in terms ofsequence?
