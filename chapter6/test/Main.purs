module Test.Main where

import Prelude
import Data.Hashable
import Control.Monad.Eff.Console

main = do
  print (hash 123)
  print (hash true)
  print (hash [1, 2, 3])
  print (hash "testing")
  print (hash 'a')

  print ("foo" `hashEqual` "foo")
  print ("foo" `hashEqual` "bar")

  print "-- hashing chars"
  print (hash 'a')
  print ('a' `hashEqual` 'a')
  print ('b' `hashEqual` 'a')

  print "-- hashing strings"
  print (hash "foo")

  print "-- hashing booleans"
  print (hash false)
  print ([true, false, false] `hashEqual` [true, false, false])
  print ([false, true, false] `hashEqual` [true, false, false])

  print "-- checking duplicates"
  print (hasDuplicates ["first", "second"])
  print (hasDuplicates ["first", "first", "second"])
  print (hasDuplicates [true, false])
  print (hasDuplicates [true, true])
  print (hasDuplicates [false])
  print (hasDuplicates ([] :: Array Boolean))


