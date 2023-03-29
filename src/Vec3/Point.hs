module Vec3.Point where

import Control.DeepSeq
import Vec3.Vec3 (Vec3)
import Vec3.Vec3Ops

data Point = Point {px :: Double, py :: Double, pz :: Double} deriving (Eq, Show)

instance NFData Point where
  rnf Point {px = x, py = y, pz = z} = x `seq` y `seq` z `seq` ()

instance Vec3Ops Point where
  x = px
  y = py
  z = pz
  constructor = Point

point x y z = Point x y z

pointToVec :: Point -> Vec3
pointToVec = convert

vecToPoint :: Vec3 -> Point
vecToPoint = convert