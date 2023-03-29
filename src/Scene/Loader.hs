module Scene.Loader where

import Hit.Hit
import qualified Hit.Hittable as Hittable
import Numeric (readHex)
import Object.Material
import Object.Sphere
import Parser
import Vec3.Color
import Vec3.Point
import Vec3.Vec3
data LoadingError = FileNotFound | ParseFailed ParseError deriving (Eq, Show)

-- | Parses a double
--
-- >>> runParser doubleParser "1.2"
-- Success (1.2,"")
--
-- >>> runParser doubleParser "1"
-- Success (1.0,"")
--
-- >>> runParser doubleParser "-1"
-- Success (-1.0,"")
--
-- >>> runParser doubleParser "-1.5"
-- Success (-1.5,"")
doubleParser :: Parser Double
doubleParser = pMap p $ sign `andThen` (digits `andThen` (opt (dot `andThen` digits)))
  where
    sign = opt $ char '-'
    dot = char '.'
    digits = some digit
    p (Nothing, n) = pNum n
    p (Just _, n) = negate $ pNum n
    pNum (num, Nothing) = read num
    pNum (num, Just (_, dec)) = read (num ++ "." ++ dec)

-- | Parses a Vec3
--
-- >>> runParser vecParser "1,2,3"
-- Success (Vec3 {vx = 1.0, vy = 2.0, vz = 3.0},"")
--
-- >>> runParser vecParser "1.0005,2.0005,3.0005"
-- Success (Vec3 {vx = 1.0005, vy = 2.0005, vz = 3.0005},"")
--
-- >>> runParser vecParser "1.0005, 2.0005, 3.0005"
-- Success (Vec3 {vx = 1.0005, vy = 2.0005, vz = 3.0005},"")
vecParser :: Parser Vec3
vecParser = 
  pMap (\(x,y,z) -> vec x y z) ( andThen3 ( doubleParser) (namedParser "," doubleParser) (namedParser "," doubleParser))


-- vecParser = error "implement vecParser"

pointParser :: Parser Point
pointParser = pMap vecToPoint vecParser

-- | Parses a color
--
-- >>> runParser colorParser "#00FF00"
-- Success (Color {r = 0.0, g = 1.0, b = 0.0},"")
--
-- >>> runParser colorParser "#ffffff"
-- Success (Color {r = 1.0, g = 1.0, b = 1.0},"")

-- colorParser :: Parser Color
-- colorParser = do
--   _ <- char '#'
--   r <- pMap (\x -> fromIntegral x / 255) . pMap fst . readHex =<< pRepeat 2 hexDigit
--   g <- pMap (\x -> fromIntegral x / 255) . pMap fst . readHex =<< pRepeat 2 hexDigit
--   b <- pMap (\x -> fromIntegral x / 255) . pMap fst . readHex =<< pRepeat 2 hexDigit
--   return $ color r g b
--   where
--     hexDigit :: Parser Char
--     hexDigit = oneOf $ ['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F']
-- hexColorParser :: Parser Double
-- hexColorParser = 
--   pMap (\hexCode -> fromIntegral (fst . head . readHex $ hexCode) / 255) (pRepeat 2 hexDigit)
--   where
--     hexDigit = letter `orElse` digit

-- colorParser :: Parser Color
-- colorParser = 
--   pMap (\(r,g,b) -> color r g b)  (andThen3(char '#' `pThen` hexColorParser) (char ',' `pThen` hexColorParser) (char ',' `pThen` hexColorParser))

hexColorParser :: Parser Double
hexColorParser = 
  pMap (\hexCode -> fromIntegral (fst . head . readHex $ hexCode) / 255) (pRepeat 2 hexDigit)
  where
    hexDigit = letter `orElse` digit

colorParser :: Parser Color
colorParser = 
  pMap (\(r,g,b) -> color r g b) (andThen3 (char '#' `pThen` hexColorParser) (hexColorParser) (hexColorParser))



-- colorParser = error "implement colorParser"

-- >>> runParser (namedParser "ir" doubleParser) "ir 1.2"
-- Success (1.2,"")
namedParser :: String -> Parser a -> Parser a
namedParser name p = string name `pThen` ws `pThen` p

-- >>> p = dict1Parser id ("ir", doubleParser)
-- >>> runParser p "{ir 2}"
-- Success (2.0,"")
dict1Parser :: (a -> b) -> (String, Parser a) -> Parser b
dict1Parser fn (nameA, pa) = between (char '{' `pThen` ws) (ws `pThen` char '}') kvs
  where
    kvs = pMap fn (namedParser nameA pa)

-- >>> data Light = Light Point Color deriving (Show)
-- >>> p = dict2Parser Light ("pos", pointParser) ("color", colorParser)
-- >>> runParser p "{ pos -20,0,0 color #AAAAAA }"
-- Success (Light (Point {px = -20.0, py = 0.0, pz = 0.0}) (Color {r = 0.6666666666666666, g = 0.6666666666666666, b = 0.6666666666666666}),"")
dict2Parser :: (a -> b -> c) -> (String, Parser a) -> (String, Parser b) -> Parser c
dict2Parser fn (nameA, pa) (nameB, pb) = pDict
  where
    pDict = between (char '{' `pThen` ws) (ws `pThen` char '}') kvs
    kvs = pMap2 fn (namedParser nameA pa) (ws `pThen` namedParser nameB pb)

-- >>> p = dict3Parser Sphere ("pos", pointParser) ("radius", doubleParser) ("material", materialParser)
-- >>> runParser p "{ pos 0,0,0 radius 10 material { diffuse {color #880000}}}"
-- Success (Sphere {sphereCenter = Point {px = 0.0, py = 0.0, pz = 0.0}, sphereRadius = 10.0, sphereMaterial = Diffuse (Lambertian {lambertianAlbedo = Color {r = 0.5333333333333333, g = 0.0, b = 0.0}})},"")
dict3Parser :: (a -> b -> c -> d) -> (String, Parser a) -> (String, Parser b) -> (String, Parser c) -> Parser d
dict3Parser fn (nameA, pa) (nameB, pb) (nameC, pc) = pDict
  where
    pDict = between (char '{' `pThen` ws) (ws `pThen` char '}') kvs
    kvs = pMap3 fn (namedParser nameA pa) (between ws ws $ namedParser nameB pb) (namedParser nameC pc)

dict4Parser :: (a -> b -> c -> d -> e) -> (String, Parser a) -> (String, Parser b) -> (String, Parser c) -> (String, Parser d) -> Parser e
dict4Parser fn (nameA, pa) (nameB, pb) (nameC, pc) (nameD, pd) = pDict
  where
    pDict = between (char '{' `pThen` ws) (ws `pThen` char '}') kvs
    kvs =
      pMap4
        fn
        (namedParser nameA pa)
        (between ws ws $ namedParser nameB pb)
        (between ws ws $ namedParser nameC pc)
        (namedParser nameD pd)

dict5Parser :: (a -> b -> c -> d -> e -> f) -> (String, Parser a) -> (String, Parser b) -> (String, Parser c) -> (String, Parser d) -> (String, Parser e) -> Parser f
dict5Parser fn (nameA, pa) (nameB, pb) (nameC, pc) (nameD, pd) (nameE, pe) = pDict
  where
    pDict = between (char '{' `pThen` ws) (ws `pThen` char '}') kvs
    kvs =
      pMap5
        fn
        (namedParser nameA pa)
        (between ws ws $ namedParser nameB pb)
        (between ws ws $ namedParser nameC pc)
        (between ws ws $ namedParser nameD pd)
        (namedParser nameE pe)

dict6Parser :: (a -> b -> c -> d -> e -> f -> g) -> (String, Parser a) -> (String, Parser b) -> (String, Parser c) -> (String, Parser d) -> (String, Parser e) -> (String, Parser f) -> Parser g
dict6Parser fn (nameA, pa) (nameB, pb) (nameC, pc) (nameD, pd) (nameE, pe) (nameF, pf) = pDict
  where
    pDict = between (char '{' `pThen` ws) (ws `pThen` char '}') kvs
    kvs =
      pMap6
        fn
        (namedParser nameA pa)
        (between ws ws $ namedParser nameB pb)
        (between ws ws $ namedParser nameC pc)
        (between ws ws $ namedParser nameD pd)
        (between ws ws $ namedParser nameE pe)
        (namedParser nameF pf)

-- >>> runParser sphereParser "sphere {center 0,0,0 radius 2 mat {diffuse {color #00FF00}}}"
-- Success (Sphere {sphereCenter = Point {px = 0.0, py = 0.0, pz = 0.0}, sphereRadius = 2.0, sphereMaterial = Diffuse (Lambertian {lambertianAlbedo = Color {r = 0.0, g = 1.0, b = 0.0}})},"")
sphereParser :: Parser Sphere
sphereParser = namedParser "sphere" $ dict3Parser Sphere ("center", pointParser) ("radius", doubleParser) ("mat", materialParser)

objectParser :: Parser Hittable.Object
objectParser = oneOf (pMap Hittable.Sphere sphereParser) []

-- >>> runParser materialParser "{diffuse {color #00FF00}}"
-- Success (Diffuse (Lambertian {lambertianAlbedo = Color {r = 0.0, g = 1.0, b = 0.0}}),"")
--
-- >>> runParser materialParser "{metallic {color #00FF00}}"
-- Success (Metal (Metallic {metallicAlbedo = Color {r = 0.0, g = 1.0, b = 0.0}, metallicFuzz = 0.0}),"")
--
-- >>> runParser materialParser "{metallic {color #00FF00 fuzz 1}}"
-- Success (Metal (Metallic {metallicAlbedo = Color {r = 0.0, g = 1.0, b = 0.0}, metallicFuzz = 1.0}),"")
--
-- >>> runParser materialParser "{dielectric {ir 0.5}}"
-- Success (Glass (Dielectric {dielectricIr = 0.5}),"")
--
-- >>> runParser materialParser "{glass {ir 0.5}}"
-- Error (UnexpectedInput {gotInput = "{glass {ir 0.5}}", expectedInput = "diffuse material or metallic material or dielectric material"})
materialParser :: Parser Material
materialParser = error "impelement materialParser"
-- materialParser =
--   let
--     str = fst <$> (between (char '{') (char '{') (many letter))
--   in 
--   case str of
--     "diffuse" -> dict1Parser Diffuse ("color" colorParser)
--     "glass" -> 