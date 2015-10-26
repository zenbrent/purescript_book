module Exercises.Transformers where

import Prelude

import Control.Monad.Error.Class

import Control.Monad.Except.Trans

import Control.Monad.State
import Control.Monad.State.Class
import Control.Monad.State.Trans

import Control.Monad.Trans

import Control.Monad.Writer
import Control.Monad.Writer.Class
import Control.Monad.Writer.Trans

import Data.Either
import Data.Identity
import Data.String (drop, take, toUpper, toLower)

{--
split :: StateT String (Either String) String
split = do
    s <- get
    case s of
         "" -> lift $ Left "Empty string"
         _ -> do
             put (drop 1 s)
             return (take 1 s)
--}

{--
runStateT split ""
runStateT split "asdf"
runStateT ((++) <$> split <*> split) "asdf"
runStateT ((++) <$> split <*> split) ""
--}


writerAndExceptT :: ExceptT String (Writer (Array String)) String
writerAndExceptT = do
  lift $ tell ["Before the error"]
  throwError "Error!"
  lift $ tell ["After the error"]
  return "Return value"


{--
runWriter $ runExceptT writerAndExceptT
--}


type Errors = Array String

type Log = Array String

type Parser = StateT String (WriterT Log (ExceptT Errors Identity))

{--
split :: Parser String
split = do
  s <- get
  lift $ tell ["The state is " ++ show s]
  case s of
    "" -> lift $ lift $ throwError ["Empty string"]
    _ -> do
      put (drop 1 s)
      return (take 1 s)
--}

split :: Parser String
split = do
  s <- get
  tell ["The state is " ++ show s]
  case s of
    "" -> throwError ["Empty string"]
    _ -> do
      put (drop 1 s)
      return (take 1 s)

{--
let runParser p s = runIdentity $ runExceptT $ runWriterT $ runStateT p s

runParser split "test"
runParser ((++) <$> split <*> split) "asdf"
runParser ((++) <$> split <*> split) ""

import qualified Split as S
import Control.Alternative
import Data.List hiding (drop, take)

-- run the split computation until it fails:
runParser (many split) "test"

--}

import Control.MonadPlus

upper :: Parser String
upper = do
    s <- split
    guard $ toUpper s == s
    return s

lower :: Parser String
lower = do
    s <- split
    guard $ toLower s == s
    return s

{--
runParser upper "ASDFasdf"
runParser lower "ASDFasdf"
runParser upper "asdfASDF"
runParser lower "asdfASDF"

import Control.Alt
let upperOrLower = some upper <|> some lower

runParser upperOrLower "abcDEFghiJKL"
let components = many upperOrLower
runParser components "abcDEFghiJKL"
--}
