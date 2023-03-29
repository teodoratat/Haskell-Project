module Vec3.Color where

import Bmp.Bmp (Pixel, pixel)
import qualified Debug.Trace as Debug
import Util.Util
import Vec3.Vec3Ops

data Color = Color {r :: Double, g :: Double, b :: Double} deriving (Eq, Show)

instance Vec3Ops Color where
  x = r
  y = g
  z = b
  constructor = Color

-- vecToColor :: Vec3 -> Int -> Color
-- vecToColor v1 nrSamples = Color $ Vec3 vx vy vz
--   where
--     sf = fromIntegral nrSamples
--     cl = clamp 0 0.999
--     vx = 255 * cl (sqrt $ (x v1) / sf)
--     vy = 255 * cl (sqrt $ (y v1) / sf)
--     vz = 255 * cl (sqrt $ (z v1) / sf)

-- colorToVec :: Color -> Vec3
-- colorToVec (Color c) =
--   Vec3 (x c / 255) (y c / 255) (z c / 255)

colorToPixel :: Color -> Int -> Pixel
colorToPixel c nrSamples = px
  where
    -- Debug.traceShow (show c ++ " " ++ show px) px

    smps = fromIntegral nrSamples
    cl = clamp 0 0.999
    vx = 255 * cl (sqrt $ (r c) / smps)
    vy = 255 * cl (sqrt $ (g c) / smps)
    vz = 255 * cl (sqrt $ (b c) / smps)
    px = pixel (round vx) (round vy) (round vz)

color :: Double -> Double -> Double -> Color
color = Color