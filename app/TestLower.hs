module Main where

-- RVRS Internal Modules
import RVRS.Parser (parseRVRS)
-- import RVRS.Lower (mergeAndLower)
import RVRS.Eval (evalIRFlow)
import qualified RVRS.AST as AST

-- System / Standard Libraries
import System.Environment (getArgs)
import qualified Data.Map as Map
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      source <- readFile filename
      case parseRVRS source of
        Left parseErr -> do
          putStrLn "[FAIL] Parse Error:\n"
          putStrLn (errorBundlePretty parseErr)
        Right flows -> do
          let flowEnv = mergeAndLower flows
          putStrLn "\n[INFO] Evaluation Output:"
          result <- evalIRFlow flowEnv "main" []
          case result of
            Left err -> putStrLn $ "[FAIL] Evaluation error: " ++ show err
            Right (Just val) -> putStrLn $ "[PASS] Returned: " ++ show val
            Right Nothing -> putStrLn "[PASS] Flow completed with no return"
    _ -> putStrLn "Usage: testlower <file>.rvrs"
