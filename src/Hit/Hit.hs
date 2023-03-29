module Hit.Hit where

import Control.DeepSeq
import Data.List
import Hit.HitRecord
import Object.AABB
import Object.Material
import Ray
import Util.Random
import Vec3.Color
import Vec3.Vec3
import Vec3.Vec3Ops

class Hittable a where
  hit :: Ray -> a -> Double -> Double -> Maybe HitRecord
  boundingBox :: a -> Maybe AABB

scatter :: Material -> Ray -> HitRecord -> Rnd Random (Maybe (Color, Ray))
scatter (Diffuse mat) _ hitRec = do
  v <- randomVecInUnitSphere
  let p = hitPoint hitRec
      n = hitNormal hitRec
      scatterDir = n `add` v
      d = if nearZero scatterDir then n else scatterDir
  return $ Just (lambertianAlbedo mat, Ray p d)
scatter (Metal mat) ray hitRec = do
  rndU <- randomVecInUnitSphere
  let refl = reflect (unit (rayDirection ray)) (hitNormal hitRec)
      fuzz = metallicFuzz mat
      scattered = Ray (hitPoint hitRec) (refl `add` (rndU `timesConst` fuzz))
  if ((rayDirection scattered) `dot` (hitNormal hitRec)) > 0
    then return $ Just (metallicAlbedo mat, scattered)
    else return Nothing
scatter (Glass mat) ray hitRec = do
  rndD <- nextDouble
  let attenuation = color 1 1 1
      ir = dielectricIr mat
      refractionRatio = if hitFrontFace hitRec then 1 / ir else ir
      unitDirection = unit $ rayDirection ray
      cos_theta = min ((neg unitDirection) `dot` (hitNormal hitRec)) 1.0
      sin_theta = sqrt (1 - cos_theta ^ 2)
      cannot_refract = refractionRatio * sin_theta > 1
      refracted =
        if cannot_refract || (reflectance cos_theta refractionRatio > rndD)
          then reflect unitDirection (hitNormal hitRec)
          else refract unitDirection (hitNormal hitRec) refractionRatio
      scattered = Ray (hitPoint hitRec) refracted
      reflectance cosine refIdx =
        let r0 = (1 - refIdx) / (1 + refIdx)
            r0' = r0 ^ 2
         in r0' + (1 - r0') * ((1 - cosine) ^ 5)
   in return $ Just (attenuation, scattered)
