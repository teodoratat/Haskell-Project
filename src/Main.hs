module Main where

import Args
import Bmp.Bmp
import Control.Applicative
import qualified Data.ByteString.Builder as BSB
import Data.List
import Data.Maybe
import Image
import Render
import Result
import Scene
import Scene.Loader
import qualified System.Environment as ENV
import System.IO
import Util.Random

generateScene :: Handle -> Image -> Scene -> IO ()
generateScene hOutFile image scene =
  let img = renderScene image scene newSeed
   in BSB.hPutBuilder hOutFile $ createBitmap img

main :: IO ()
main = do
  argList <- ENV.getArgs
  case procArgs argList of
    Error err -> putStrLn "Failed to parse args"
    Success args -> do
      let outFile = fromMaybe "render.bmp" $ argOutFile args
          nrSamples = argNrSamples args
      imageConfig <- getImageConfig (argImageConfigFile args)
      sceneConfig <- getSceneConfig (argSceneFile args)
      case (imageConfig, sceneConfig) of
        (Error err, _) -> putStrLn $ "Failed to load image config: " ++ show err
        (_, Error err) -> putStrLn $ "Failed to load scene config: " ++ show err
        (Success imageConfig, Success sceneConfig) -> do
          let updateImageConfig :: Maybe Int -> Image -> Image
              updateImageConfig nrSamples image = maybe image (\s -> image {imageNrSamples = s}) nrSamples

              newImageConfig = updateImageConfig (argNrSamples args) imageConfig
              scene = runRandom newSeed $ buildScene newImageConfig sceneConfig
          hOutFile <- openBinaryFile outFile WriteMode
          generateScene hOutFile imageConfig scene
          hFlush hOutFile
          hClose hOutFile
