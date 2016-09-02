module Main where

import Prelude hiding (readFile)

import Data.Text.IO (readFile)

import System.Environment (getArgs)
import Text.Megaparsec (parse, parseErrorPretty)

import Ruby.Parser (parseProgram)

main :: IO ()
main = do
  files <- getArgs
  (flip mapM_ files) $ \file -> do
    result <- parse parseProgram file <$> readFile file
    putStrLn $ case result of
      Left msg -> parseErrorPretty msg
      Right _ -> "The file: " ++ file ++ " parsed."
