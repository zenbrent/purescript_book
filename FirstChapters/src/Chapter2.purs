module Chapter2 where

import Debug.Trace
import Math

-- main = trace "Hello, world!"

diagonal w h = sqrt (w * w + h * h)

-- calculate the area of a circle based on a given radius.
circleArea r = pi * Math.pow r 2

main = print (diagonal 3 4)
