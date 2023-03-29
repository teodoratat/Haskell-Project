module Object.Sphere where

import Control.DeepSeq
import Hit.Hit
import Hit.HitRecord
import Object.AABB
import Object.Material
import Ray
import Vec3.Point
import Vec3.Vec3
import Vec3.Vec3Ops

data Sphere = Sphere {sphereCenter :: Point, sphereRadius :: Double, sphereMaterial :: Material} deriving (Eq, Show)

instance NFData Sphere where
  rnf (Sphere {sphereCenter = center, sphereRadius = radius, sphereMaterial = mat}) = force center `seq` radius `seq` force mat `seq` ()

instance Hittable Sphere where
  hit = hitSphere
  boundingBox s = Just $ AABB minP maxP
    where
      center = pointToVec $ sphereCenter s
      radius = sphereRadius s
      rVec = (vec radius radius radius)
      minP = vecToPoint $ center `sub` rVec
      maxP = vecToPoint $ center `add` rVec

hitSphere :: Ray -> Sphere -> Double -> Double -> Maybe HitRecord
hitSphere ray sphere minT maxT =
  let oc = (pointToVec $ rayOrigin ray) `sub` (pointToVec $ sphereCenter sphere)
      rDir = rayDirection ray
      sR = sphereRadius sphere
      a = lenSqrd rDir
      halfB = (oc `dot` rDir)
      c = lenSqrd oc - sR * sR
      discriminant = halfB * halfB - a * c
   in if discriminant < 0
        then Nothing
        else
          let between x lo hi = lo <= x && x <= hi
              sqrtd = sqrt discriminant
              root1 = (- halfB - sqrtd) / a
              root2 = (- halfB + sqrtd) / a

              mkRec root =
                Just $ hitRecord root p ray outN (sphereMaterial sphere)
                where
                  p = ray `at` root
                  outN = ((pointToVec p) `sub` (pointToVec $ sphereCenter sphere)) `divideConst` (sphereRadius sphere)
           in if between root1 minT maxT
                then mkRec root1
                else
                  if between root2 minT maxT
                    then mkRec root2
                    else Nothing