module Main where

-- 🌊 RVRS Internal Modules
import RVRS.Parser (parseRVRS)
import RVRS.Lower (mergeAndLower)
import RVRS.Eval (evalIRFlow)
import RVRS.Value (Value(..))
import qualified RVRS.AST as AST
-- import qualified RVRS.IR as IR

-- 📦 System / Standard Libraries
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
          putStrLn "❌ Parse Error:\n"
          putStrLn (errorBundlePretty parseErr)
        Right flows -> do
          let flowEnv = mergeAndLower flows
          putStrLn "\n🌊 Evaluation Output:"
          result <- evalIRFlow flowEnv "main" []
          case result of
            Left err -> putStrLn $ "❌ Evaluation error: " ++ show err
            Right (Just val) -> putStrLn $ "✅ Returned: " ++ show val
            Right Nothing -> putStrLn "(✅ Flow completed with no return)"
    _ -> putStrLn "Usage: testlower <file>.rvrs"
