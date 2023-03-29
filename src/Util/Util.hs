module Util.Util where

import Result
import System.Directory

clamp :: Double -> Double -> Double -> Double
clamp lo hi n
  | n < lo = lo
  | n > hi = hi
  | otherwise = n

inf = 1 / 0 :: Double

degToRad :: Double -> Double
degToRad d = d * pi / 180