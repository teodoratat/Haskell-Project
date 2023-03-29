module Vec3.Vec3 where

import Vec3.Vec3Ops

data Vec3 = Vec3 {vx :: Double, vy :: Double, vz :: Double} deriving (Eq, Show)

vec :: Double -> Double -> Double -> Vec3
vec x y z = Vec3 x y z

instance Vec3Ops Vec3 where
  x = vx
  y = vy
  z = vz
  constructor = Vec3

reflect :: Vec3 -> Vec3 -> Vec3
reflect v n = v `sub` (n `timesConst` (2 * dot v n))

refract :: Vec3 -> Vec3 -> Double -> Vec3
refract uv n etaiOverEtat =
  let cosTheta = min ((neg uv) `dot` n) 1.0
      rOutPerp = (uv `add` (n `timesConst` cosTheta)) `timesConst` etaiOverEtat
      rOutParallel = n `timesConst` (- sqrt (abs (1 - lenSqrd rOutPerp)))
   in rOutPerp `add` rOutParallel

nearZero :: Vec3 -> Bool
nearZero (Vec3 x y z) = (abs x < s) && (abs y < s) && (abs z < s) where s = 0.00000001
