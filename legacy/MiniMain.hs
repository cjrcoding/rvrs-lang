module Main where

import MiniParser
import Text.Megaparsec (errorBundlePretty)
import System.Environment (getArgs)
import System.IO (readFile)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      content <- readFile file
      case parseRVRS content of
        Left err -> putStrLn (errorBundlePretty err)
        Right flows -> print flows
    _ -> putStrLn "Usage: miniparser <filename>"
