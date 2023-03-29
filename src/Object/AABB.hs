{-# LANGUAGE BangPatterns #-}

module Object.AABB where

import Control.DeepSeq
import Data.List
import Debug.Trace (traceShow, traceShowId)
import Ray
import Vec3.Point
import Vec3.Vec3
import Vec3.Vec3Ops

data AABB = AABB {aabbMin :: Point, aabbMax :: Point} deriving (Eq, Show)

instance NFData AABB where
  rnf AABB {aabbMin = min, aabbMax = max} = force min `seq` force max `seq` ()

aabb :: Point -> Point -> AABB
aabb = AABB

emptyAABB = AABB o o where o = point 0 0 0

hitAABB :: Ray -> AABB -> Double -> Double -> Bool
hitAABB r aabb tMinP tMaxP =
  fst $ foldl' f (True, (tMinP, tMaxP)) [0 .. 2]
  where
    f (False, (tMin', tMax')) _ = (False, (tMin', tMax'))
    f (s, (tMin', tMax')) i =
      let idx = get i
          idxPoint = get i . pointToVec
          invD = 1 / idx (rayDirection r)
          minCoord = idx $ pointToVec (aabbMin aabb)
          maxCoord = idx $ pointToVec (aabbMax aabb)
          origCoord = idxPoint (rayOrigin r)
          t0' = (minCoord - origCoord) * invD
          t1' = (maxCoord - origCoord) * invD
          t0 = if invD < 0 then t1' else t0'
          t1 = if invD < 0 then t0' else t1'
          tMin = max t0 tMin'
          tMax = min t1 tMax'
       in (s && tMax > tMin, (tMin, tMax))

sorroundingBox :: AABB -> AABB -> AABB
sorroundingBox a b =
  AABB small big
  where
    x' = x . pointToVec
    y' = y . pointToVec
    z' = z . pointToVec
    minA = aabbMin a
    minB = aabbMin b
    maxA = aabbMax a
    maxB = aabbMax b
    small = point (min (x' minA) (x' minB)) (min (y' minA) (y' minB)) (min (z' minA) (z' minB))
    big = point (max (x' maxA) (x' maxB)) (max (y' maxA) (y' maxB)) (max (z' maxA) (z' maxB))