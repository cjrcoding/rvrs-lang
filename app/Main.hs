-- app/Main.hs

module Main where

import RVRS.Parser
import RVRS.AST
import System.Environment (getArgs)
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      content <- readFile filename
      case parseRVRS content of
        Left err  -> putStrLn $ errorBundlePretty err
        Right ast -> putStrLn $ prettyPrintFlow ast
    _ -> putStrLn "Usage: rvrs <file>.rvrs"

-- | Define how a Flow prints in a more readable format
prettyPrintFlow :: Flow -> String
prettyPrintFlow (Flow name args body) =
  "Flow\n  name: " ++ name ++
  "\n  args: " ++ show args ++
  "\n  body:\n    " ++ unlines (map show body)
