module Main where

import System.Directory (listDirectory)
import System.FilePath ((</>), takeExtension)
import RVRS.Parser (parseRVRS)
import RVRS.Lower (lowerFlow)
import RVRS.EvalIR (evalIRFlow)
import RVRS.Value (Value(..))
import RVRS.AST (flowName)
import Data.List (find)
import Control.Monad (forM_)

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
      Right flows -> case find (\f -> flowName f == "main") flows of
        Nothing -> putStrLn "❌ No 'main' flow found."
        Just flow -> do
          let lowered = lowerFlow flow
          putStrLn "✅ Lowered IR:"
          print lowered
          putStrLn "🔁 Evaluation Output:"
          _ <- evalIRFlow lowered []
          return ()
