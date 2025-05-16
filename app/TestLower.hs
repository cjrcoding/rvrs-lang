-- app/TestLower.hs
module Main where

import RVRS.Parser (parseRVRS)
import RVRS.Lower (lowerFlow)
import Text.Megaparsec (parse)
import System.Environment (getArgs)
import Control.Monad (mapM_)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      content <- readFile filePath
      case parseRVRS content of
        Left err -> putStrLn $ "❌ Parse error:\n" ++ show err
        Right flows -> do
          let lowered = map lowerFlow flows
          putStrLn "✅ Lowered IR:"
          mapM_ print lowered
    _ -> putStrLn "Usage: cabal run TestLower path/to/file.rvrs"
