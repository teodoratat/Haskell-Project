module Hit.Hittable where

import Control.DeepSeq
import Data.List
import qualified Hit.Hit as Hit
import Hit.HitRecord
import qualified Object.AABB as AABB
import Object.BVHNode
import Object.Material
import qualified Object.Sphere as Sphere
import Ray
import Vec3.Point

data Object = Sphere Sphere.Sphere | BVHTree (BVHTree Object) deriving (Eq, Show)

hit :: Ray -> Object -> Double -> Double -> Maybe HitRecord
hit r h tMin tMax = case h of
  Sphere sp -> Hit.hit r sp tMin tMax
  BVHTree bt -> Hit.hit r bt tMin tMax

boundingBox :: Object -> Maybe AABB.AABB
boundingBox (Sphere sp) = Hit.boundingBox sp
boundingBox (BVHTree bt) = Hit.boundingBox bt

instance Hit.Hittable Object where
  hit r o tMin tMax = hit r o tMin tMax
  boundingBox o = boundingBox o

hitObject :: Ray -> Object -> Double -> Double -> Maybe HitRecord
hitObject r o tMin tMax = hit r o tMin tMax

hitList :: Ray -> [Object] -> Double -> Double -> Maybe HitRecord
hitList r objs tMin tMax =
  fst $ foldl' foldFn start objs
  where
    foldFn (cRec, cMax) obj =
      case hit r obj tMin cMax of
        Nothing -> (cRec, cMax)
        Just hitRec ->
          (Just hitRec, hitT hitRec)
    start = (Nothing, tMax)

instance NFData Object where
  rnf o = rnf o

sphere :: Point -> Double -> Material -> Object
sphere c r m = Sphere $ Sphere.Sphere c r m