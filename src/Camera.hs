module Camera where

import Bmp.Bmp
import Debug.Trace
import Hit.Hit
import Hit.HitRecord
import Image
import Object.Material
import Object.Sphere
import Parser
import Ray
import Scene.Loader
import Util.Random
import Util.Util
import Vec3.Color
import Vec3.Point
import Vec3.Vec3
import Vec3.Vec3Ops

data CameraConfig = CameraConfig
  { camConfigLookFrom :: Point,
    camConfigLookAt :: Point,
    camConfigVUp :: Vec3,
    camConfigVertFOV :: Double,
    camConfigAperture :: Double,
    camConfigFocusDist :: Double
  }
  deriving (Eq, Show)

data Camera = Camera
  { cameraOrigin :: Point,
    cameraHorizontal :: Vec3,
    cameraVertical :: Vec3,
    cameraLowerLeftCorner :: Point,
    cameraU :: Vec3,
    cameraW :: Vec3,
    cameraV :: Vec3,
    cameraLensRadius :: Double
  }
  deriving (Show)

setupCamera :: Double -> CameraConfig -> Camera
setupCamera aspectRatio config =
  let theta = degToRad (camConfigVertFOV config)
      h = tan (theta / 2)
      vpHeight = 2 * h
      vpWidth = aspectRatio * vpHeight
      w = pointToVec $ unit ((camConfigLookFrom config) `sub` (camConfigLookAt config))
      u = unit ((camConfigVUp config) `cross` w)
      v = w `cross` u
      orig = (camConfigLookFrom config)
      horizontal = u `timesConst` (vpWidth * (camConfigFocusDist config))
      vertical = v `timesConst` (vpHeight * (camConfigFocusDist config))
      llc = vecToPoint $ (pointToVec orig) `sub` (horizontal `divideConst` 2) `sub` (vertical `divideConst` 2) `sub` (w `timesConst` (camConfigFocusDist config))
   in Camera
        { cameraOrigin = orig,
          cameraHorizontal = horizontal,
          cameraVertical = vertical,
          cameraLowerLeftCorner = llc,
          cameraU = u,
          cameraW = w,
          cameraV = v,
          cameraLensRadius = (camConfigAperture config) / 2
        }

cameraRay :: Camera -> Double -> Double -> Rnd Random Ray
cameraRay camera s t = do
  rndUDisk <- randomInUnitDisk
  let rd = rndUDisk `timesConst` (cameraLensRadius camera)
      u = cameraU camera
      v = cameraV camera
      offset = (u `timesConst` (x rd)) `add` (v `timesConst` (y rd))
      o = cameraOrigin camera
      llc = cameraLowerLeftCorner camera
      camH = cameraHorizontal camera
      camV = cameraVertical camera
      rOrig = o `add` (vecToPoint offset)
      rDir = (pointToVec llc) `add` (camH `timesConst` s) `add` (camV `timesConst` t) `sub` (pointToVec o) `sub` (offset)
   in return $ Ray rOrig rDir

-- >>> runParser cameraParser "camera {lookFrom 0,2,0.5 lookAt 0,0.5,-0.2 v_up 0,1,0 vert_fov 90 aperture 0.2 focus_distance 1.5}"
cameraParser :: Parser CameraConfig
cameraParser =
  namedParser "camera" $
    dict6Parser
      (CameraConfig)
      ("look_from", pointParser)
      ("look_at", pointParser)
      ("v_up", vecParser)
      ("vert_fov", doubleParser)
      ("aperture", doubleParser)
      ("focus_distance", doubleParser)