module Args where

import Data.List
import Result
import Text.Read (readMaybe)
import Parser
data Args = Args
  { argImageConfigFile :: Maybe String,
    argSceneFile :: Maybe String,
    argOutFile :: Maybe String,
    argNrSamples :: Maybe Int
  }
  deriving (Eq, Show)

data ParseArgsError = InvalidArgs deriving (Eq, Show)

type ArgMap = [(String, String)]

-- >>> toArgMap ["-x", "y"]
-- Success [("x","y")]
--
-- >>> toArgMap ["-x", "y", "-a", "b"]
-- Success [("x","y"),("a","b")]
--
-- >>> toArgMap ["x", "y"]
-- Error InvalidArgs
--
-- >>> toArgMap ["-x", "y", "-z"]
-- Error InvalidArgs
toArgMapHelp :: [String] -> ArgMap
toArgMapHelp args = 
    [(dropWhile (=='-') k, v) | (k, v) <- zip args (drop 1 args), '-' == head k]

-- toArgMap :: [String] -> Result ParseArgsError ArgMap
-- toArgMap [] = Success []
-- toArgMap str = 
--   let 
--     len = (length str) `mod` 2
--     isLengthDivisibleByTwo = len == 0
--   in
--     case isLengthDivisibleByTwo of
--       True -> Error InvalidArgs
--       False -> if (len == (length (toArgMapHelp str))) then Success (toArgMapHelp str)
--             else Error InvalidArgs

toArgMap :: [String] -> Result ParseArgsError ArgMap
toArgMap [] = Success []
toArgMap str = 
  let
    l = (length str) `mod` 2
    len = ((length str) `div` 2)
  in
    case l of
      1 -> Error InvalidArgs
      0 -> if ( len == (length (toArgMapHelp str))) then Success (toArgMapHelp str)
            else Error InvalidArgs


-- >>> getArg "key" [("key", "value")]
-- Just "value"

getArg :: String -> ArgMap -> Maybe String
getArg key argMap = 
    case filter (\(k, _) -> k == key) argMap of
        [] -> Nothing
        (x:_) -> Just (snd x)

-- >>> readArg "name" [("name", "1")] :: Maybe Int
-- Just 1
--
-- >>> readArg "name" [("name", "one")] :: Maybe Int
-- Nothing
--
-- >>> readArg "number" [("name", "1")] :: Maybe Int
-- Nothing
readArg :: (Read a) => String -> ArgMap -> Maybe a
readArg key argMap = do
    case find (\(k,v) -> k == key) argMap of
        Just (_,value) -> readMaybe value
        Nothing -> Nothing
    where readMaybe s = case reads s of
                            [(x, "")] -> Just x
                            _ -> Nothing

-- procArgs :: [String] -> Result ParseArgsError Args
-- procArgs args = do
--   argMap <- toArgMap args
--   let
--     imageConfigFile = getArg "-imageConfigFile" argMap
--     sceneFile = getArg "-sceneFile" argMap
--     outFile = getArg "-outFile" argMap
--     nrSamples = readArg "-nrSamples" argMap
--   in
--     case (imageConfigFile, sceneFile, outFile, nrSamples) of
--         (Just imgCfgFile, Just scnFile, Just oFile, Just nrSmp) ->  Success (Args (Just imgCfgFile) (Just scnFile) (Just oFile) (Just nrSmp))
--         _ -> Error InvalidArgs

toArgMapFunc :: Result ParseArgsError ArgMap -> ArgMap
toArgMapFunc (Success x) = x
toArgMapFunc _ = [("empty", "em")]
procArgs :: [String] -> Result ParseArgsError Args
procArgs args =
  let argMap = toArgMap args
      argMapFunc = toArgMapFunc argMap
   in case argMap of
        Error InvalidArgs -> Error InvalidArgs
        Success _ ->
          Success
            Args
              { argImageConfigFile = getArg "imageConfigFile" argMapFunc,
                argSceneFile = getArg "sceneFile" argMapFunc,
                argOutFile = getArg "outFile" argMapFunc,
                argNrSamples = readArg "imageNrSamples" argMapFunc
              }