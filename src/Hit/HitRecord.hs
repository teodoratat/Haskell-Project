module Hit.HitRecord where

import Object.Material
import Ray
import Vec3.Point
import Vec3.Vec3
import Vec3.Vec3Ops

data HitRecord = HitRecord
  { hitPoint :: Point,
    hitNormal :: Vec3,
    hitT :: Double,
    hitFrontFace :: Bool,
    hitMaterial :: Material
  }
  deriving (Show)

hitRecord :: Double -> Point -> Ray -> Vec3 -> Material -> HitRecord
hitRecord t point ray outNormal mat =
  let ff = (rayDirection ray) `dot` outNormal < 0
      n = if ff then outNormal else neg outNormal
   in HitRecord
        { hitPoint = point,
          hitT = t,
          hitFrontFace = ff,
          hitNormal = n,
          hitMaterial = mat
        }