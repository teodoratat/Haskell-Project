module Scene where

import Bmp.Bmp (BmpImg)
import Camera
import Control.Applicative (liftA2, liftA3)
import Control.DeepSeq
import qualified Hit.Hit as Hit
import Hit.Hittable
import qualified Hit.Hittable as Hittable
import Image
import Object.BVHNode
import Object.Material
import Object.Sphere
import Parser
import Result
import Scene.Loader
import Test.SimpleTest.Mock (TestableMonadIO (..))
import Util.Random
import Vec3.Color
import Vec3.Point
import Vec3.Vec3
import Vec3.Vec3Ops
import Prelude hiding (readFile)

data SceneConfig = SceneConfig
  { sceneConfigCameraConfig :: CameraConfig,
    sceneConfigObjects :: [Hittable.Object]
  }
  deriving (Eq, Show)

data Scene = Scene
  { sceneCamera :: Camera,
    sceneBVH :: BVHTree Hittable.Object
  }
  deriving (Show)

buildScene :: Image -> SceneConfig -> Rnd Random Scene
buildScene image sceneConfig = do
  bvh <- buildBVH (sceneConfigObjects sceneConfig)
  let camera = setupCamera (imageAspectRatio image) (sceneConfigCameraConfig sceneConfig)
  return $ Scene {sceneCamera = camera, sceneBVH = bvh}

defaultWorld :: [Hittable.Object]
defaultWorld =
  let groundSphere = sphere (point 0 (-100) (-1)) 100 (diffuse $ color 0.8 0.8 0.8)
      threeSpheres midPoint radius distance mat1 mat2 mat3 =
        [ sphere (mapX (\c -> c - (2 * radius + distance)) midPoint) radius mat1, -- left
          sphere (midPoint) radius mat2, -- middle
          sphere (mapX (\c -> c + (2 * radius + distance)) midPoint) radius mat3 -- right
        ]
   in mconcat
        [ threeSpheres (point 0 0.5 (0.2)) 0.5 0.2 (metal $ color 0.1 0.1 0.8) (diffuse $ color 0.7 0.1 0.1) (fuzzyMetal (color 0.8 0.8 0.8) 0.3),
          threeSpheres (point 0 0.5 (-1)) 0.5 0.2 (metal $ color 0.8 0.6 0.2) (diffuse $ color 0.1 0.7 0.1) (glass 1.5),
          threeSpheres (point 0 0.5 (-2.2)) 0.5 0.2 (glass 2.5) (metal $ color 0.3 0.7 0.7) (fuzzyMetal (color 0.7 0.1 0.7) 0.5),
          [ -- sphere (point 0.5 0.1 (-1)) 0.1 (diffuse $ color 0.8 0 0.8), -- small purple
            -- sphere (point (-0.5) 0.1 (-1)) 0.1 (glass 1.5), -- small glass
            groundSphere
          ]
        ]

defaultSceneConfig :: SceneConfig
defaultSceneConfig =
  let camConfig = CameraConfig (point 0 2 0.5) (point 0 0.5 (-0.2)) (vec 0 1 0) 90 0.2 1.5
   in SceneConfig {sceneConfigCameraConfig = camConfig, sceneConfigObjects = defaultWorld}

sceneParser :: Parser SceneConfig
sceneParser = pMap2 SceneConfig cameraParser (ws `pThen` (sepBySome ws objectParser))

getSceneConfig :: TestableMonadIO io => Maybe String -> io (Result LoadingError SceneConfig)
getSceneConfig _ = error "implement getSceneConfig"

loadSceneConfig :: TestableMonadIO io => String -> io (Result LoadingError SceneConfig)
loadSceneConfig _ = error "implement loadSceneConfig"