module Render where

import Bmp.Bmp
import Camera
import Control.DeepSeq
import Debug.Trace
import Hit.Hit
import Hit.HitList
import Hit.HitRecord
import Hit.Hittable (sphere)
import qualified Hit.Hittable as Hittable
import Image
import Object.BVHNode
import Object.Material
import Object.Sphere
import Ray
import Scene
import Util.Random
import Util.Util
import Vec3.Color
import Vec3.Point
import Vec3.Vec3
import Vec3.Vec3Ops

rayColor :: Hittable a => Ray -> a -> Int -> Rnd Random Color
rayColor _ _ 0 = return $ color 0 0 0
rayColor r world depth = do
  case hit r world 0.001 inf of
    Just hitRec -> do
      let mat = hitMaterial hitRec
      sc <- scatter mat r hitRec
      case sc of
        Nothing -> return $ color 0 0 0
        Just (att, sc) -> do
          rc <- rayColor sc world (depth - 1)
          return $ att `times` rc
    Nothing -> do
      let d = unit (rayDirection r)
          t = 0.5 * (y d + 1.0)
          interP = timesConst (color 1 1 1) (1 - t) `add` timesConst (color 0.5 0.7 1.0) t
      return interP

renderScene :: Image -> Scene -> Random -> BmpImg
renderScene image scene seed =
  let Image {imageHeight = h, imageWidth = w, imageNrSamples = nrSamples, imageMaxDepth = maxDepth} = image
      Scene {sceneCamera = camera, sceneBVH = bvh} = scene
      genPixel :: Int -> Int -> Rnd Random Pixel
      genPixel x y = do
        let xf = fromIntegral x :: Double
            yf = fromIntegral y :: Double

            aaSample :: Int -> Color -> Rnd Random Color
            aaSample 0 acc = do return acc
            aaSample smp acc = do
              r1 <- nextDouble
              r2 <- nextDouble
              let u = (xf + r1) / fromIntegral (imageWidth image - 1)
                  v = (yf + r2) / fromIntegral (imageHeight image - 1)
              r <- cameraRay camera u v
              c <- rayColor r bvh maxDepth
              aaSample (smp - 1) (acc `add` c)
        smpColor <- aaSample nrSamples (color 0 0 0)
        return $ colorToPixel smpColor nrSamples

      gen :: Int -> Int -> [[Pixel]]
      gen w h = [[runRandom (varyRandom seed (y + x)) (genPixel x y) | x <- [0 .. w -1]] | y <- [0 .. h -1]]
   in BmpImg $ map ImgRow (gen w h)