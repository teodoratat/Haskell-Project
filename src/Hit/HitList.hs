module Hit.HitList where

import Data.List
import qualified Hit.Hit as Hit
import Hit.HitRecord
import Hit.Hittable
import Object.AABB
import Object.Material
import Ray
import Util.Random
import Vec3.Point
import Vec3.Vec3
import Vec3.Vec3Ops

instance Hit.Hittable a => Hit.Hittable [a] where
  hit r objs tMin tMax =
    fst $ foldl' foldFn start objs
    where
      foldFn (cRec, cMax) obj =
        case Hit.hit r obj tMin cMax of
          Nothing -> (cRec, cMax)
          Just hitRec ->
            (Just hitRec, hitT hitRec)
      start = (Nothing, tMax)

  boundingBox [] = Nothing
  boundingBox (o : os) =
    foldl' foldFn (Hit.boundingBox o) os
    where
      foldFn Nothing o = Nothing
      foldFn (Just aabb) o = case Hit.boundingBox o of
        Nothing -> Nothing
        Just oAABB -> Just $ sorroundingBox oAABB aabb

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