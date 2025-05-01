module Main where

import RVRS.Parser (parseRVRS)
import RVRS.Codegen (prettyPrintFlow)
import RVRS.Eval (evalFlow)
import RVRS.AST (Flow, flowName)
import System.Environment (getArgs)
import qualified Data.Map as M
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = do
  putStrLn "🟢 Running REAL Main.hs (from src/app/Main.hs)"
  args <- getArgs
  case args of
    [filename] -> do
      content <- readFile filename
      case parseRVRS content of
        Left err -> putStrLn $ errorBundlePretty err
        Right flows -> do
          let flowEnv = M.fromList [(flowName f, f) | f <- flows]
          case M.lookup "main" flowEnv of
            Just mainFlow -> do
              putStrLn "Parsed Flow:\n"
              putStrLn (prettyPrintFlow mainFlow)
              putStrLn "\nEvaluation Output:"
              
              -- 🔧 STARTING WITH A SCOPED ENVIRONMENT STACK
              result <- evalFlow flowEnv mainFlow [M.empty]

              case result of
                Just val -> putStrLn ("Returned: " ++ show val)
                Nothing  -> putStrLn "(Flow completed with no return)"
            Nothing ->
              putStrLn "Error: No flow named 'main' found."
    _ -> putStrLn "Usage: rvrs <file>.rvrs"
