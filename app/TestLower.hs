module Main where

import RVRS.Parser (parseRVRS)
import RVRS.Lower (lowerFlow)
import RVRS.EvalIR (evalIRFlow)
import RVRS.Value (Value(..))
import RVRS.AST (flowName)  -- Import flowName accessor
import Text.Megaparsec (parse)
import System.Environment (getArgs)
import Data.List (find)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      content <- readFile filePath
      case parseRVRS content of
        Left err -> putStrLn $ "❌ Parse error:\n" ++ show err
        Right flows -> case find (\f -> flowName f == "main") flows of
          Nothing -> putStrLn "❌ No 'main' flow found."
          Just flow -> do
            let lowered = lowerFlow flow
            putStrLn "✅ Lowered IR:"
            print lowered
            putStrLn "🔁 Evaluation Output:"
            _ <- evalIRFlow lowered []
            return ()
    _ -> putStrLn "Usage: cabal run TestLower path/to/file.rvrs"
