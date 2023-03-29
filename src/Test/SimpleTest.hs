{-# LANGUAGE GADTs #-}

module Test.SimpleTest where

import Control.Exception
import Data.Fixed (Centi, Fixed (MkFixed))
import qualified Data.List as L
import Test.SimpleTest.Expectation
import Test.SimpleTest.TestCase
import Text.Printf (printf)

group :: (Show a) => String -> [TestTree a] -> TestTree a
group name items = TestTreeNode {testGroupName = name, testGroupChildren = items}

testCase :: Expectation e => String -> Int -> e -> TestTree TestCase
testCase name points expectation =
  TestTreeLeaf TestCase {testCaseName = name, testCaseExpectation = expectation, testCasePoints = MkFixed (fromIntegral points)}

runTest :: TestCase -> Bool -> IO TestResult
runTest tc@(TestCase _ expectation _) showDetails = do
  res <- try $ evaluate $ holds expectation :: IO (Either ErrorCall Bool)
  case res of
    Left (ErrorCallWithLocation message location) -> return $ Todo tc message location showDetails
    Right res ->
      if res
        then return $ Passed tc
        else return $ Failed tc showDetails

runTestGroup :: Bool -> TestTree TestCase -> IO (TestTree TestResult)
runTestGroup showDetails (TestTreeLeaf tc) = TestTreeLeaf <$> (runTest tc showDetails)
runTestGroup showDetails (TestTreeNode name tts) = TestTreeNode name <$> sequenceA (fmap (runTestGroup showDetails) tts)

calculateScore :: TestTree TestResult -> TestPoints Centi
calculateScore (TestTreeLeaf (Passed tc)) = TestPoints pts pts where pts = testCasePoints tc
calculateScore (TestTreeLeaf (Failed tc _)) = TestPoints 0 pts where pts = testCasePoints tc
calculateScore (TestTreeLeaf (Todo tc _ _ _)) = TestPoints 0 pts where pts = testCasePoints tc
calculateScore (TestTreeNode s tts) = foldMap calculateScore tts

evalTestGroup :: Bool -> TestTree TestCase -> IO ()
evalTestGroup showDetails gr = do
  results <- runTestGroup showDetails gr
  let score = calculateScore results
  print results
  printf "Final score: %s\n" (show score)
