module Test.Tests where

import Args (Args (..), ParseArgsError (InvalidArgs), getArg, procArgs, readArg, toArgMap)
import Camera (CameraConfig (CameraConfig), setupCamera)
import Control.Monad
import Control.Monad.State (State, execState)
import Data.Maybe (listToMaybe)
import Hit.Hittable (sphere)
import Image (defaultImage, getImageConfig, imageParser, loadImageConfig, setupImage)
import qualified Main
import Object.Material (diffuse, fuzzyMetal, glass, metal)
import qualified Object.Sphere as Sphere
import Parser
import Result (Result (..), isError)
import Scene
import Scene.Loader
import System.Environment (getArgs)
import qualified Test.Data as Data
import Test.SimpleTest
import Test.SimpleTest.Expectation
import Test.SimpleTest.Mock (makeMockIOState)
import qualified Test.SimpleTest.Mock as Mock
import Test.SimpleTest.TestCase
import Test.Util
import Text.Printf (printf)
import Vec3.Color (color)
import Vec3.Point (point)
import Vec3.Vec3 (vec)

tests =
  [ ("parser", parserTests),
    ("args", argTests),
    ("io", ioTests)
  ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> evalTestGroup False testSuite
    args' ->
      let tg' = do
            name <- listToMaybe $ dropWhile (`elem` ["-d", "--detailed"]) args'
            lookup name tests
          detailed = any (`elem` ["-d", "--detailed"]) args'
       in case tg' of
            Nothing -> evalTestGroup detailed testSuite
            Just tg -> evalTestGroup detailed tg

testSuite :: TestTree TestCase
testSuite =
  group
    "test suite"
    (map snd tests)

parserTests :: TestTree TestCase
parserTests =
  group
    "parser tests"
    [ group
        "vecParser"
        ( let shouldParseAs = mkShouldParseAs vecParser
           in [ testCase "parses ints" 50 ("0,0,0" `shouldParseAs` (vec 0 0 0)),
                testCase "parses doubles" 50 ("1.0005,2.0005,3.0005" `shouldParseAs` (vec 1.0005 2.0005 3.0005)),
                testCase "parses ints with spaces" 50 ("0, 0, 0" `shouldParseAs` (vec 0 0 0)),
                testCase "parses doubles with spaces" 50 ("1.0005, 2.0005, 3.0005" `shouldParseAs` (vec 1.0005 2.0005 3.0005))
              ]
        ),
      group
        "colorParser"
        ( let shouldParseAs = mkShouldParseAs colorParser
           in [ testCase "parses #000000" 50 ("#000000" `shouldParseAs` (color 0 0 0)),
                testCase "parses #ffffff" 50 ("#ffff00" `shouldParseAs` (color 1 1 0)),
                testCase "parses #FFFFFF" 50 ("#FFFFFF" `shouldParseAs` (color 1 1 1))
              ]
        ),
      group
        "materialParser"
        ( let shouldParseAs = mkShouldParseAs materialParser
           in [ testCase "parses diffuse" 50 ("{diffuse {color #00FF00}}" `shouldParseAs` (diffuse (color 0 1 0))),
                testCase "parses metallic" 50 ("{metallic {color #0000FF}}" `shouldParseAs` (metal (color 0 0 1))),
                testCase "parses fuzzy metallic" 50 ("{metallic {color #FF0000 fuzz 1}}" `shouldParseAs` (fuzzyMetal (color 1 0 0) 1)),
                testCase "parses dielectric" 50 ("{dielectric {ir 0.5}}" `shouldParseAs` (glass 0.5))
              ]
        ),
      group
        "imageParser"
        ( let shouldParseAs = mkShouldParseAs imageParser
           in [ testCase "parses image" 50 ("image {width 800 height 450 nr_samples 100 max_depth 50}" `shouldParseAs` setupImage 800 450 100 50)
              ]
        )
    ]

argTests :: TestTree TestCase
argTests =
  group
    "argument processing tests"
    [ group
        "argument parsing"
        [ testCase
            "empty arguments are parsed successfully"
            25
            (toArgMap [] `shouldBe` Success []),
          testCase
            "one argument pair is parsed successfully"
            50
            (toArgMap ["-x", "y"] `shouldBe` Success [("x", "y")]),
          testCase
            "multiple argument pairs are parsed successfully"
            50
            (toArgMap ["-x", "y", "-a", "b", "-name", "arg"] `shouldBe` Success [("x", "y"), ("a", "b"), ("name", "arg")]),
          testCase
            "parsing fails when a value is missing"
            50
            (toArgMap ["-x"] `shouldBe` Error InvalidArgs),
          testCase
            "parsing fails when the first key doesn't start with '-'"
            50
            (toArgMap ["x", "y"] `shouldBe` Error InvalidArgs),
          testCase
            "parsing fails when a key doesn't start with '-'"
            50
            (toArgMap ["-x", "y", "a", "b"] `shouldBe` Error InvalidArgs)
        ],
      group
        "argument reading"
        [ testCase
            "present string value"
            50
            (getArg "key" [("key", "value")] `shouldBe` Just "value"),
          testCase
            "absent string value"
            25
            (getArg "key" [("a", "b")] `shouldBe` Nothing),
          testCase
            "present int value"
            50
            (readArg "key" [("key", "5")] `shouldBe` Just 5),
          testCase
            "absent int value"
            25
            (readArg "key" [("a", "b")] `shouldBe` (Nothing :: Maybe Int)),
          testCase
            "invalid int value"
            50
            (readArg "key" [("key", "one")] `shouldBe` (Nothing :: Maybe Int))
        ],
      group
        "argument processing"
        [ testCase
            "empty arguments"
            25
            ( procArgs []
                `shouldBe` Success
                  Args
                    { argImageConfigFile = Nothing,
                      argOutFile = Nothing,
                      argSceneFile = Nothing,
                      argNrSamples = Nothing
                    }
            ),
          testCase
            "imageNrSamples argument"
            25
            ( procArgs ["-imageNrSamples", "50"]
                `shouldBe` Success
                  Args
                    { argImageConfigFile = Nothing,
                      argOutFile = Nothing,
                      argSceneFile = Nothing,
                      argNrSamples = Just 50
                    }
            ),
          testCase
            "outFile argument"
            25
            ( procArgs ["-outFile", "render.bmp"]
                `shouldBe` Success
                  Args
                    { argImageConfigFile = Nothing,
                      argOutFile = Just "render.bmp",
                      argSceneFile = Nothing,
                      argNrSamples = Nothing
                    }
            ),
          testCase
            "multiple arguments"
            50
            ( procArgs ["-outFile", "render.bmp", "-imageNrSamples", "100"]
                `shouldBe` Success
                  Args
                    { argImageConfigFile = Nothing,
                      argOutFile = Just "render.bmp",
                      argSceneFile = Nothing,
                      argNrSamples = Just 100
                    }
            )
        ]
    ]

ioTests :: TestTree TestCase
ioTests =
  group
    "io tests"
    [ group
        "loadImageConfig"
        [ testCase "reads from file when it exists" 50 (shouldReadFromFile "image.cfg" testFs (loadImageConfig "image.cfg")),
          testCase "fails when file is missing" 50 (shouldHaveResult testFs (loadImageConfig "img.cfg") (Error FileNotFound)),
          testCase
            "fails when file is corrupted"
            50
            ( resultShouldSatisfy
                testFsCorrupedFile
                (loadImageConfig "image.cfg")
                ( \res -> case res of
                    Error (ParseFailed _) -> True
                    _ -> False
                )
            )
        ],
      group
        "getImageConfig"
        ( let imageConfig = setupImage 1600 800 200 75
           in [ testCase "parses configuration from file" 50 (shouldHaveResult testFs (getImageConfig (Just "image.cfg")) (Success imageConfig)),
                testCase "returns default configuration if no file is given" 50 (shouldHaveResult testFs (getImageConfig Nothing) (Success defaultImage)),
                testCase "fails when file is missing" 50 (shouldHaveResult testFs (getImageConfig (Just "img.cfg")) (Error FileNotFound))
              ]
        ),
      group
        "loadSceneConfig"
        [ testCase "reads from file when it exists" 50 (shouldReadFromFile "scene.crt" testFs (loadSceneConfig "scene.crt")),
          testCase "fails when file is missing" 50 (shouldHaveResult testFs (loadSceneConfig "scene.txt") (Error FileNotFound)),
          testCase
            "fails when file is corrupted"
            50
            ( resultShouldSatisfy
                testFsCorrupedFile
                (loadSceneConfig "scene.crt")
                ( \res -> case res of
                    Error (ParseFailed _) -> True
                    _ -> False
                )
            )
        ],
      group
        "getSceneConfig"
        ( let cameraConfig = CameraConfig (point 0 2 0.5) (point 0 0.5 0) (vec 0 1 0) 90 0.2 1.5
              sceneObjects = [sphere (point 0 0.5 0.2) 0.5 (glass 0.5)]
           in [ testCase "parses configuration from file" 50 (shouldHaveResult testFs (getSceneConfig (Just "scene.crt")) (Success (SceneConfig cameraConfig sceneObjects))),
                testCase "returns default configuration if no file is given" 50 (shouldHaveResult testFs (getSceneConfig Nothing) (Success defaultSceneConfig)),
                testCase "fails when file is missing" 50 (shouldHaveResult testFs (getSceneConfig (Just "scene.cfg")) (Error FileNotFound))
              ]
        )
    ]

testFs =
  makeMockIOState
    [ ("scene.crt", cameraConfig ++ "\n" ++ sphereConfig),
      ("image.cfg", "image {width 1600 height 800 nr_samples 200 max_depth 75}")
    ]
  where
    cameraConfig = "camera {\n    look_from 0,2,0.5\n    look_at 0,0.5,0\n    v_up 0,1,0\n    vert_fov 90\n    aperture 0.2\n    focus_distance 1.5\n}"
    sphereConfig = "sphere {\n    center 0,0.5,0.2\n    radius 0.5\n    mat {\n        dielectric {\n            ir 0.5\n        }\n    }\n}"

testFsCorrupedFile =
  makeMockIOState
    [ ("scene.crt", "invalid"),
      ("image.cfg", "invalid")
    ]
