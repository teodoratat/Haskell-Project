module Image where

import Parser
import Result
import Scene.Loader
import Test.SimpleTest.Mock (TestableMonadIO (..))
import Prelude hiding (readFile)

data Image = Image
  { imageAspectRatio :: Double,
    imageWidth :: Int,
    imageHeight :: Int,
    imageNrSamples :: Int,
    imageMaxDepth :: Int
  }
  deriving (Eq, Show)

setupImage :: Int -> Int -> Int -> Int -> Image
setupImage w h nrSamples maxDepth =
  Image
    { imageAspectRatio = fromIntegral w / fromIntegral h,
      imageWidth = w,
      imageHeight = h,
      imageNrSamples = nrSamples,
      imageMaxDepth = maxDepth
    }

defaultImage :: Image
defaultImage = setupImage 800 450 100 50

-- >>> runParser imageParser "image {width 800 height 450 nr_samples 100 max_depth 50}"
-- Success (Image {imageAspectRatio = 1.7777777777777777, imageWidth = 800, imageHeight = 450, imageNrSamples = 100, imageMaxDepth = 50},"")
imageParser :: Parser Image
imageParser = namedParser "image" $ dict4Parser (\width height nrSamples maxDepth -> (setupImage width height nrSamples maxDepth)) ("width", number) ("height", number) ("nr_samples", number) ("max_depth", number)
-- >>> p = dict3Parser Sphere ("pos", pointParser) ("radius", doubleParser) ("material", materialParser)
-- >>> runParser p "{ pos 0,0,0 radius 10 material { diffuse {color #880000}}}"

getImageConfig :: TestableMonadIO io => Maybe String -> io (Result LoadingError Image)
getImageConfig _ = error "implement getImageConfig"

loadImageConfig :: TestableMonadIO io => String -> io (Result LoadingError Image)
loadImageConfig _ = error "implement loadImageConfig"