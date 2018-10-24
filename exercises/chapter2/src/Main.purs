module Main where

import Prelude ((+), (*))
import Effect.Console (logShow)
import Math (sqrt)

diagonal :: Number -> Number -> Number
diagonal w h = sqrt (w * w + h * h)

main = logShow (diagonal 3.0 4.0)
