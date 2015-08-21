module Notes where

import Prelude

import Data.Maybe
import Data.Array
import Data.Either
import Data.Validation
import Data.AddressBook
import Data.Traversable

import qualified Data.String as S
import qualified Data.String.Regex as R

import Control.Apply

import Data.AddressBook

{--
instance functorMaybe :: Functor Maybe where
    map f (Just a) = Just (f a)
    map f Nothing = Nothing

instance applyMaybe :: Apply Maybe where
    apply (Just f) (Just x) = Just (f x)
    apply _ _ = Nothing
--}



