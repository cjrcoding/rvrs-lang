module Main where

import RVRS.Parser (parseRVRS)
import RVRS.Codegen (prettyPrintFlow)
import RVRS.Eval (evalFlow)
import System.Environment (getArgs)
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      content <- readFile filename
      case parseRVRS content of
        Left err -> putStrLn $ errorBundlePretty err
        Right ast -> do
          putStrLn "Parsed Flow:\n"
          putStrLn (prettyPrintFlow ast)
          putStrLn "\nEvaluation Output:"
          result <- evalFlow ast
          case result of
            Just val -> putStrLn ("Returned: " ++ show val)
            Nothing  -> putStrLn "(Flow completed with no return)"
    _ -> putStrLn "Usage: rvrs <file>.rvrs"
