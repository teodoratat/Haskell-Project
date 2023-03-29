{-# LANGUAGE FlexibleInstances #-}

module Test.SimpleTest.Mock where

-- Based on https://stackoverflow.com/questions/7370073/testing-functions-in-haskell-that-do-io/7374754
-- and https://lexi-lambda.github.io/blog/2017/06/29/unit-testing-effectful-haskell-with-monad-mock/

import Control.Monad.State (State, get, modify, put)
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified System.Directory as Dir
import qualified Test.SimpleTest.Color as Color
import Text.Printf (printf)

class Monad m => TestableMonadIO m where
  readFile :: FilePath -> m String
  writeFile :: FilePath -> String -> m ()
  putStrLn :: String -> m ()
  print :: Show a => a -> m ()
  doesFileExist :: FilePath -> m Bool

data FileState = FileState
  { fileStateContents :: [String],
    fileStateRead :: Bool,
    fileStateWritten :: Bool,
    fileStateNew :: Bool
  }

instance Show FileState where
  show (FileState {fileStateContents = contents, fileStateRead = read, fileStateWritten = written, fileStateNew = new}) =
    printf "[%s%s%s]: \n%s\n%s\n%s" n r w line (head contents) line
    where
      line = replicate 80 '~'
      r = if read then "r" else " "
      w = if written then "w" else " "
      n = if new then "*" else " "

data MockIOState = MockIOState
  { mockFsState :: Map.Map String FileState,
    mockStdoutState :: [String],
    mockFilesChecked :: Set.Set String
  }

instance Show MockIOState where
  show (MockIOState {mockFsState = fs, mockStdoutState = stdout}) =
    printf "FS:\n%sSTDOUT:\n%s\n%s\n%s" files line output line
    where
      line = replicate 80 '~'
      files = unlines (map (\(n, v) -> printf "  %s: %s" (Color.blue n) (show v)) (Map.toList fs))
      output = unlines stdout

makeMockIOState :: [(String, String)] -> MockIOState
makeMockIOState files =
  MockIOState
    { mockFsState = Map.fromList (map (fmap makeMockFile) files),
      mockStdoutState = [],
      mockFilesChecked = Set.empty
    }

makeMockFile :: String -> FileState
makeMockFile contents =
  FileState
    { fileStateContents = [contents],
      fileStateRead = False,
      fileStateWritten = False,
      fileStateNew = False
    }

fileSatisfies :: (FileState -> Bool) -> String -> MockIOState -> Bool
fileSatisfies p path state =
  let fs = mockFsState state
      file = Map.lookup path fs
   in case file of
        Nothing -> False
        (Just file') -> p file'

fileHasContents :: String -> FilePath -> MockIOState -> Bool
fileHasContents contents = fileSatisfies p
  where
    p file =
      head (fileStateContents file) == contents

fileContentsContains :: String -> FilePath -> MockIOState -> Bool
fileContentsContains substr = fileSatisfies p
  where
    p file =
      substr `L.isInfixOf` head (fileStateContents file)

fileWasWritten :: FilePath -> MockIOState -> Bool
fileWasWritten = fileSatisfies fileStateWritten

fileWasRead :: FilePath -> MockIOState -> Bool
fileWasRead = fileSatisfies fileStateRead

fileWasCreated :: FilePath -> MockIOState -> Bool
fileWasCreated = fileSatisfies fileStateNew

fileWasChecked :: FilePath -> MockIOState -> Bool
fileWasChecked path (MockIOState {mockFilesChecked = filesChecked}) = Set.member path filesChecked

fileExists :: FilePath -> MockIOState -> Bool
fileExists path state =
  let fs = mockFsState state
   in Map.member path fs

stdoutContains :: String -> MockIOState -> Bool
stdoutContains msg state =
  let stdout = mockStdoutState state
   in msg `elem` stdout

instance TestableMonadIO (State MockIOState) where
  readFile path = do
    st@MockIOState {mockFsState = fs} <- get
    let v = Map.lookup path fs
    case v of
      Nothing -> error "File not found"
      (Just file) -> do
        put $ st {mockFsState = Map.insert path (file {fileStateRead = True}) fs}
        return $ head $ fileStateContents file
  writeFile path contents = do
    st@MockIOState {mockFsState = fs} <- get
    let v = Map.lookup path fs
    case v of
      Nothing ->
        put $
          st
            { mockFsState =
                Map.insert
                  path
                  ( FileState
                      { fileStateWritten = True,
                        fileStateContents = [contents],
                        fileStateRead = False,
                        fileStateNew = True
                      }
                  )
                  fs
            }
      (Just file) -> do
        put $
          st
            { mockFsState =
                Map.insert
                  path
                  ( file
                      { fileStateWritten = True,
                        fileStateContents = contents : fileStateContents file
                      }
                  )
                  fs
            }
    return ()
  putStrLn str = do
    st@MockIOState {mockStdoutState = sc} <- get
    put st {mockStdoutState = sc <> lines str}
  print a = do
    st@MockIOState {mockStdoutState = sc} <- get
    put st {mockStdoutState = sc <> lines (show a)}
  doesFileExist path = do
    st@MockIOState {mockFsState = fs, mockFilesChecked = filesChecked} <- get
    put st {mockFilesChecked = Set.insert path filesChecked}
    let v = Map.lookup path fs
    return $ case v of
      Nothing -> False
      (Just _) -> True

instance TestableMonadIO IO where
  readFile = Prelude.readFile
  writeFile = Prelude.writeFile
  putStrLn = Prelude.putStrLn
  print = Prelude.print
  doesFileExist = Dir.doesFileExist
