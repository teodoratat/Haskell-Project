{-# LANGUAGE BangPatterns #-}

module Object.BVHNode where

import Control.DeepSeq
import Data.List (sortBy)
import Data.Maybe (fromJust)
import Debug.Trace (traceShow, traceShowId)
import Hit.Hit
import Hit.HitRecord
import Object.AABB
import Ray
import Util.Random
import Vec3.Point
import Vec3.Vec3Ops

data BVHTree a
  = BVHNode {bvhNodeLeft :: BVHTree a, bvhNodeRight :: BVHTree a, bvhNodeBox :: AABB}
  | BVHLeaf a
  | BVHNil
  deriving (Show)

instance (Eq a) => Eq (BVHTree a) where
  BVHNil == BVHNil = True
  BVHLeaf l == BVHLeaf r = l == r
  BVHNode ll lr lb == BVHNode rl rr rb = ll == rl && lr == rr && lb == rb
  _ == _ = False

instance NFData (BVHTree a) where
  rnf BVHNil = ()
  rnf (BVHLeaf a) = seq a ()
  rnf (BVHNode {bvhNodeBox = box, bvhNodeLeft = leftNode, bvhNodeRight = rightNode}) =
    rnf box `seq` rnf leftNode `seq` rnf rightNode `seq` ()

instance (Hittable a) => Hittable (BVHTree a) where
  hit r (BVHNode {bvhNodeBox = box, bvhNodeLeft = leftNode, bvhNodeRight = rightNode}) tMin tMax =
    if hitAABB r box tMin tMax
      then
        let leftHit = hit r leftNode tMin tMax
         in case leftHit of
              Nothing -> hit r rightNode tMin tMax
              Just hr ->
                case hit r rightNode tMin (hitT hr) of
                  Nothing -> Just hr
                  Just hr' -> Just hr'
      else Nothing
  hit r (BVHLeaf a) tMin tMax = hit r a tMin tMax
  hit _ BVHNil _ _ = Nothing

  boundingBox node@(BVHNode _ _ _) = Just $ bvhNodeBox node
  boundingBox (BVHLeaf a) = boundingBox a
  boundingBox BVHNil = Just emptyAABB

buildBVH :: (Hittable a, NFData a) => [a] -> Rnd Random (BVHTree a)
buildBVH [] = return BVHNil
buildBVH [o] = return $ BVHLeaf o
buildBVH !objects = do
  let sortFn :: Hittable a => (Point -> Double) -> a -> a -> Ordering
      sortFn axis o1 o2 =
        let min1 = (axis . aabbMin) <$> boundingBox o1
            min2 = (axis . aabbMin) <$> boundingBox o2
         in compare (fromJust min1) (fromJust min2)
  axis <- randomSample x [y, z]
  let sorted = sortBy (sortFn axis) objects
      mid = length sorted `div` 2
      (left, right) = splitAt mid sorted
  case (left, right) of
    ([l], [r]) -> do
      let box = sorroundingBox (fromJust $ boundingBox l) (fromJust $ boundingBox r)
      return $ BVHNode (BVHLeaf l) (BVHLeaf r) box
    _ -> do
      leftNode <- buildBVH left
      rightNode <- buildBVH right
      let box = sorroundingBox (fromJust $ boundingBox leftNode) (fromJust $ boundingBox rightNode)
      return $ BVHNode leftNode rightNode box