module Main where

-- Internal modules
import RVRS.Parser (parseRVRS)
import RVRS.Lower (lowerFlow)
import RVRS.EvalIR (evalIRFlow)
import RVRS.Value (Value(..))
import qualified RVRS.AST as AST
import qualified RVRS.IR as IR

-- System / standard libraries
import System.Directory (listDirectory)
import System.FilePath ((</>), takeExtension)
import Data.List (find)
import Control.Monad (forM_)
import qualified Data.Map as Map

testsDir :: FilePath
testsDir = "tests/ir"

main :: IO ()
main = do
  putStrLn "🌊 Running all IR tests..."
  files <- listDirectory testsDir
  let rvrsFiles = filter (\f -> takeExtension f == ".rvrs") files

  forM_ rvrsFiles $ \file -> do
    let path = testsDir </> file
    putStrLn $ "\n🔍 Running: " ++ path
    content <- readFile path
    case parseRVRS content of
      Left err -> putStrLn $ "❌ Parse error:\n" ++ show err
      Right flows -> do
        let loweredFlows = map lowerFlow flows
        let flowMap = Map.fromList [(IR.flowName f, f) | f <- loweredFlows]
        case find (\f -> IR.flowName f == "main") loweredFlows of
          Nothing -> putStrLn "❌ No 'main' flow found."
          Just mainFlow -> do
            putStrLn "✅ Lowered IR:"
            print mainFlow
            putStrLn "🔁 Evaluation Output:"
            result <- evalIRFlow flowMap (IR.flowName mainFlow) []
            case result of
              Right (Just val) -> putStrLn $ "✅ Flow returned: " ++ show val
              Right Nothing    -> putStrLn "✅ Flow completed without explicit return"
              Left err         -> putStrLn $ "❌ Runtime error: " ++ show err
