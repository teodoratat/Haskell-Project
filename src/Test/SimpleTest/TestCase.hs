{-# LANGUAGE GADTs #-}

module Test.SimpleTest.TestCase where

import Data.Fixed (Centi)
import Data.List (intercalate)
import qualified Data.List as L
import qualified Test.SimpleTest.Color as Color
import Test.SimpleTest.Expectation
import Text.Printf (printf)

data TestPoints a where
  TestPoints :: (Num a, Show a) => {testPointsObtained :: a, testPointsTotal :: a} -> TestPoints a

fullPoints :: (Num a, Show a) => a -> TestPoints a
fullPoints pts = TestPoints pts pts

noPoints :: (Num a, Show a) => a -> TestPoints a
noPoints pts = TestPoints 0 pts

instance Semigroup (TestPoints a) where
  (TestPoints obtainedA totalA) <> (TestPoints obtainedB totalB) = TestPoints (obtainedA + obtainedB) (totalA + totalB)

instance (Num a, Show a) => Monoid (TestPoints a) where
  mempty = TestPoints 0 0

instance Show (TestPoints a) where
  show (TestPoints obtained total) = printf "%s/%s" (show obtained) (show total)

data TestResult
  = Passed TestCase
  | Failed {failedTestCase :: TestCase, failedShowDetails :: Bool}
  | Todo {todoTestCase :: TestCase, todoMessage :: String, todoLocation :: String, todoShowDetails :: Bool}

instance Show TestResult where
  show (Passed TestCase {testCaseName = name, testCasePoints = points}) = printf "%s: %s" name (Color.green (show $ fullPoints points))
  show (Failed TestCase {testCaseName = name, testCasePoints = points, testCaseExpectation = expectation} showDetails)
    | showDetails = printf "%s: %s%s" (Color.red name) (Color.red (show $ noPoints points)) details
    | otherwise = printf "%s: %s" (Color.red name) (Color.red (show $ noPoints points))
    where
      details = printf "\n%s\n%s\n%s" startline (show expectation) endline :: String
      startline = printf "%s" (replicate 80 'v') :: String
      endline = printf "%s" (replicate 80 '^') :: String
  show (Todo TestCase {testCaseName = name, testCasePoints = points} message location showDetails)
    | showDetails = printf "%s: %s" name (Color.yellow $ printf "(%s) TODO: %s%s" (show $ noPoints points) message details)
    | otherwise = printf "%s: %s" name (Color.yellow $ printf "(%s) TODO: %s" (show $ noPoints points) message)
    where
      details = printf "\n%s\n%s\n%s" startline location endline :: String
      startline = printf "%s" (replicate 80 'v') :: String
      endline = printf "%s" (replicate 80 '^') :: String

data TestCase where
  TestCase ::
    Expectation e =>
    { testCaseName :: String,
      testCaseExpectation :: e,
      testCasePoints :: Centi
    } ->
    TestCase

data TestTree a where
  TestTreeNode :: Show a => {testGroupName :: String, testGroupChildren :: [TestTree a]} -> TestTree a
  TestTreeLeaf :: Show a => a -> TestTree a

instance Show (TestTree a) where
  show t = printTree 0 t
    where
      printTree :: Int -> TestTree a -> String
      printTree indent (TestTreeLeaf a) =
        printf
          "%s%s"
          (replicate indent ' ')
          (show a)
      printTree indent (TestTreeNode s tts) =
        printf
          "%s%s\n%s"
          (replicate indent ' ')
          s
          (intercalate "\n" (fmap (printTree (indent + 2)) tts))

instance Eq TestCase where
  (TestCase nameL _ _) == (TestCase nameR _ _) = nameL == nameR

instance Show TestCase where
  show testCase =
    printf "%s" (testCaseName testCase)

newtype TestCaseAssertionFmt a = TestCaseAssertionFmt TestCase

instance Show (TestCaseAssertionFmt a) where
  show
    ( TestCaseAssertionFmt
        TestCase
          { testCaseName = name,
            testCaseExpectation = expectation,
            testCasePoints = points
          }
      ) =
      printf
        "%s (%s points)\n%s"
        name
        (show points)
        (show expectation)
