module Main where

import RVRS.Parser (parseRVRS)
import RVRS.Lower (lowerFlow)
import RVRS.EvalIR (evalIRFlow)
import RVRS.Value (Value(..))
import qualified RVRS.IR as IR         -- Import FlowIR and flowName
import qualified RVRS.AST as AST       -- Import AST flowName for original flows
import System.Environment (getArgs)
import Data.List (find)
import qualified Data.Map as Map

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      content <- readFile filePath
      case parseRVRS content of
        Left err -> putStrLn $ "❌ Parse error:\n" ++ show err
        Right flows -> case find (\f -> AST.flowName f == "main") flows of
          Nothing -> putStrLn "❌ No 'main' flow found."
          Just flow -> do
            let lowered = lowerFlow flow
            let flowMap = Map.fromList [(IR.flowName lowered, lowered)]
            putStrLn "✅ Lowered IR:"
            print lowered
            putStrLn "🔁 Evaluation Output:"
            _ <- evalIRFlow flowMap (IR.flowName lowered) []
            return ()
    _ -> putStrLn "Usage: cabal run TestLower path/to/file.rvrs"
