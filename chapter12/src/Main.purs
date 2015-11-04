module Main where
    
import Prelude

import Types

import Data.Either

import Control.Monad.Eff
import Control.Monad.Eff.Console (log, error)
import Control.Monad.Trans
import Control.Monad.Cont.Trans

import Network.HTTP.Client
import Files

main = async do
  response1 <- readFileCont "./bower.json"
  response2 <- readFileCont "./package.json"
  sum <- ContT ((++) <$> response1 <*> response2)
  lift (either error log sum)
  where
  async :: forall eff. Async eff Unit -> Eff eff Unit
  async = flip runContT return

{--
main = async do
  response <- get "http://purescript.org"
  lift (either error log response)
  where
  async :: forall eff. Async eff Unit -> Eff eff Unit
  async = flip runContT return
--}
