-- app/Main.hs

module Main where

-- Internal modules
import RVRS.AST (Flow)
import RVRS.Parser (parseRVRS)
import RVRS.Eval (evalIRFlow, EvalError)
import RVRS.Codegen (prettyPrintFlow)
import RVRS.Lower (mergeAndLower)

-- System/environment
import System.Environment (getArgs)

-- External libraries
import Control.Monad (when)
import qualified Data.Map as M
import Text.Megaparsec (errorBundlePretty)

-- 🔧 Debug toggle
debug :: Bool
debug = False  -- Set to True to show parsed flows

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      -- Parse user file
      userContent <- readFile filename
      case parseRVRS userContent of
        Left err -> putStrLn $ errorBundlePretty err
        Right userFlows -> do

          -- Parse stdlib
          stdlibContent <- readFile "stdlib/stdlib.rvrs"
          case parseRVRS stdlibContent of
            Left stdErr -> putStrLn $ "Stdlib error:\n" ++ errorBundlePretty stdErr
            Right stdlibFlows -> do

              -- 🛠️ Combine stdlib first, then user flows (user can override)
              let allFlows = stdlibFlows ++ userFlows
              let lowered = mergeAndLower allFlows

              when debug $ do
                putStrLn "\n🔎 Parsed User Flows:"
                mapM_ (putStrLn . prettyPrintFlow) userFlows

              -- Evaluate main flow
              putStrLn "\nEvaluation Output:"
              evalResult <- evalIRFlow lowered "main" []

              case evalResult of
                Left err -> putStrLn ("Eval Error: " ++ show err)
                Right (Just val) -> putStrLn ("Returned: " ++ show val)
                Right Nothing    -> putStrLn "(Flow completed with no return)"

    _ -> putStrLn "Usage: rvrs <file>.rvrs"
