module Object.Material where

import Control.DeepSeq
import Vec3.Color

data Lambertian = Lambertian {lambertianAlbedo :: Color} deriving (Eq, Show)

data Metallic = Metallic {metallicAlbedo :: Color, metallicFuzz :: Double} deriving (Eq, Show)

data Dielectric = Dielectric {dielectricIr :: Double} deriving (Eq, Show)

data Material = Diffuse Lambertian | Metal Metallic | Glass Dielectric deriving (Eq, Show)

instance NFData Material where
  rnf (Diffuse (Lambertian {lambertianAlbedo = albedo})) = albedo `seq` ()
  rnf (Metal (Metallic {metallicAlbedo = albedo, metallicFuzz = fuzz})) = albedo `seq` fuzz `seq` ()
  rnf (Glass (Dielectric {dielectricIr = ir})) = ir `seq` ()

diffuse :: Color -> Material
diffuse albedo = Diffuse $ Lambertian albedo

metal :: Color -> Material
metal albedo = Metal $ Metallic albedo 0

fuzzyMetal :: Color -> Double -> Material
fuzzyMetal albedo fuzz = Metal $ Metallic albedo fuzz

glass :: Double -> Material
glass ir = Glass $ Dielectric ir