module Ray where

import Vec3.Point
import Vec3.Vec3
import Vec3.Vec3Ops

data Ray = Ray {rayOrigin :: Point, rayDirection :: Vec3}

at :: Ray -> Double -> Point
at r t = v
  where
    o = rayOrigin r
    v = o `add` (vecToPoint $ timesConst (rayDirection r) t)