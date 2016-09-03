module Main where

import Prelude hiding (readFile)

import Data.Text.IO (readFile)

import Control.Monad

import System.Environment (getArgs)
import Text.Megaparsec (parse, parseErrorPretty)

import Ruby.Parser (parseProgram)

successMessage :: (Integer, Integer) -> String
successMessage r = "Successfully parsed " ++ (show $ fst r) ++ " files, failed " ++ (show $ snd r) ++ " files."

main :: IO ()
main = do
  files <- getArgs
  results <- flip (flip foldM (0, 0)) files $ \(succ, fail) file -> do
    result <- parse parseProgram file <$> readFile file
    case result of
      Left msg -> putStrLn (parseErrorPretty msg) *> return (succ, fail + 1)
      Right _ -> putStrLn ("The file: " ++ file ++ " parsed.") *> return (succ + 1, fail)

  putStrLn $ successMessage results
