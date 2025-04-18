module Main where

import Text.Megaparsec (errorBundlePretty)
import RVRS.Parser
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      content <- readFile filename
      case parseRVRS content of
        Left err  -> putStrLn $ errorBundlePretty err
        Right ast -> print ast
    _ -> putStrLn "Usage: rvrs <file>.rvrs"
