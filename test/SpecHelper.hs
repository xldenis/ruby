module SpecHelper where

import Prelude hiding (readFile)

import Data.Text.IO (readFile)

import Control.Monad (filterM)
import Data.Text (Text)
import System.Directory
import System.FilePath

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec


parseFromFile p file = runParser p file <$> readFile file

filesShouldParse :: Show b => FilePath -> Parsec Dec Text b -> Spec
filesShouldParse dir p = do
  fs <- runIO $ getDirectoryPaths dir >>= filterM (doesFileExist)

  describe ("parser successfully parses files in " ++ dir) $ do
    mapM_ (\f -> do
      it ((takeFileName f) ++ " parses correctly.") $ do
        (parseFromFile (p <* eof) f) >>= shouldSucceed) fs

filesShouldFail :: Show b => FilePath -> Parsec Dec Text b -> Spec
filesShouldFail dir p = do
  fs <- runIO $ getDirectoryPaths dir >>= filterM (doesFileExist)

  describe ("parser fails to parse files in " ++ dir) $ do
    mapM_ (\f -> do
      it ((takeFileName f) ++ " fails.") $ do
        (parseFromFile (p <* eof) f) >>= shouldFail) fs

type ParserExpectation t e a = Either (ParseError t e) a -> Expectation

-- | Expectation that argument is result of a failed parser.

shouldFail :: Show a => ParserExpectation t e a
shouldFail r = case r of
  Left _ -> return ()
  Right v -> expectationFailure $
    "the parser is expected to fail, but it parsed: " ++ show v

-- | Expectation that argument is result of a succeeded parser.

shouldSucceed :: (Show a, Ord t, ShowToken t, ShowErrorComponent e) => ParserExpectation t e a
shouldSucceed r = case r of
  Left e -> expectationFailure $
    "the parser is expected to succeed, but it failed with:\n" ++
    showParseError e
  Right _ -> return ()

showParseError :: (Ord t, ShowToken t, ShowErrorComponent e) => ParseError t e -> String
showParseError = parseErrorPretty

getDirectoryPaths :: String -> IO [FilePath]
getDirectoryPaths dir =  map (\f -> replaceDirectory f dir) <$> getDirectoryContents dir
