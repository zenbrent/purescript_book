module Example.Rectangle where

import Prelude

import Data.Array ((..))
import Data.Foldable (for_)
import Data.Maybe
import Data.Int (toNumber)

import Control.Monad.Eff

import Graphics.Canvas

main = do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  setFillStyle "#0000FF" ctx

  fillPath ctx $ for_ (1 .. 3) \i -> do
    rect ctx { x: 125.0 * toNumber i
             , y: 250.0
             , w: 100.0
             , h: 100.0
           }

    return unit
