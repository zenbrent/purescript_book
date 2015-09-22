module Example.Random where

import Prelude

import Data.Maybe
import Data.Int (toNumber)
import Data.Array ((..))
import Data.Foldable (for_)

import Control.Monad.Eff
import Control.Monad.Eff.Random

import Graphics.Canvas

main = do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  setFillStyle "#aa0055" ctx
  setStrokeStyle "#000000" ctx 

  for_ (1 .. 100) \i -> do
    x <- random
    y <- random
    r <- random
    rot <- random

    let path = arc ctx 
         { x     : x + 5.0 * toNumber i
         , y     : y + 5.0 * toNumber i
         , r     : r * 50.0
         , start : 0.0
         , end   : Math.pi
         }
    
    {--
    save ctx
    rotate rot ctx
    fillPath ctx path
    restore ctx
    strokePath ctx path
    --}

    -- or
    withContext ctx $ do
      rotate (toNumber (i - 100) / 75.0) ctx
      fillPath ctx path

    withContext ctx $ do
      rotate rot ctx
      strokePath ctx path

    return unit
