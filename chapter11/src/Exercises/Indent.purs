module Exercises.Indent where

import Prelude

import Data.Foldable (traverse_)
import Control.Monad.State
import Control.Monad.State.Class
import Control.Monad.Reader
import Control.Monad.Reader.Class

type Level = Int
type Doc = Reader Level String

{-- 11.5.1
(Easy) Write a function line which renders a function at the current indentation level. Your function should have the following type:
    line :: String -> Doc
Hint: use the ask function to read the current indentation level.
--}

repeated :: String -> Int -> String
repeated _ 0 = ""
repeated s 1 = s
repeated s n | n > 1 = s ++ repeated s (n - 1)

spaces :: Int -> String
spaces = repeated " "

line :: String -> Doc
line s = do
    level <- ask
    return (spaces level ++ s)

{-- 11.5.2
(Easy) Use the local function to write a function
    indent :: Doc -> Doc
which increases the indentation level for a block of code.
--}

indent :: Doc -> Doc
indent = do
    level <- ask
    local (level + 1)

{-- 11.5.3
(Medium) Use the sequence function defined in Data.Traversable to write a function
    cat :: Array Doc -> Doc
which concatenates a collection of documents, separating them with new lines.
--}

{-- 11.5.4
(Medium) Use the runReader function to write a function
    render :: Doc -> String
which renders a document as a String.

You should now be able to use your library to write simple documents, as follows:

render $ cat 
  [ line "Here is some indented text:"
  , indent $ cat 
      [ line "I am indented"
      , line "So am I"
      , indent $ line "I am even more indented"
      ]
  ]
--}
