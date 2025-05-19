module Main where

-- Internal modules
import RVRS.AST (Flow, flowName)
import RVRS.Parser (parseRVRS)
import RVRS.Eval.Eval (evalFlow)
import RVRS.Codegen (prettyPrintFlow)

-- System/environment
import System.Environment (getArgs)

-- External libraries
import Control.Monad (when)
import qualified Data.Map as M
import Text.Megaparsec (errorBundlePretty)

-- 🔧 Debug toggle
debug :: Bool
debug = False  -- Set to True if you want to show parsed flow output

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

              -- Combine flows
              let allFlows = userFlows ++ stdlibFlows
              let flowEnv = M.fromList [(flowName f, f) | f <- allFlows]

              case M.lookup "main" flowEnv of
                Just mainFlow -> do
                  when debug $ do
                    putStrLn "Parsed Flow:\n"
                    putStrLn (prettyPrintFlow mainFlow)

                  putStrLn "\nEvaluation Output:"
                  result <- evalFlow flowEnv mainFlow [M.empty]

                  case result of
                    Just val -> putStrLn ("Returned: " ++ show val)
                    Nothing  -> putStrLn "(Flow completed with no return)"
                Nothing -> putStrLn "Error: No flow named 'main' found."

    _ -> putStrLn "Usage: rvrs <file>.rvrs"
