module Main where

-- Internal modules
import RVRS.Parser (parseRVRS)
import RVRS.Lower (lowerFlow)
import RVRS.EvalIR (evalIRFlow, EvalError)
import RVRS.Value (Value(..))
import qualified RVRS.AST as AST
import qualified RVRS.IR as IR

-- System / standard libraries
import System.Directory (listDirectory)
import System.FilePath ((</>), takeExtension)
import Data.List (find, isPrefixOf)
import Control.Monad (forM)
import Control.Exception (try, SomeException)
import qualified Data.Map as Map

testsDir :: FilePath
testsDir = "tests/ir"

data TestResult = Passed | Failed | ExpectedFail deriving (Eq)

main :: IO ()
main = do
  putStrLn "🌊 Running all IR tests..."
  files <- listDirectory testsDir
  let rvrsFiles = filter (\f -> takeExtension f == ".rvrs") files

  results <- forM rvrsFiles runIRTest
  putStrLn "\n🔚 Test Summary:"
  summarize results

runIRTest :: FilePath -> IO TestResult
runIRTest file = do
  let path = testsDir </> file
  putStrLn $ "\n🔍 Running: " ++ path
  content <- readFile path
  let expectedFail = any ("-- expect-fail" `isPrefixOf`) (lines content)

  case parseRVRS content of
    Left err -> do
      putStrLn $ "❌ Parse error:\n" ++ show err
      return Failed
    Right flows -> do
      let loweredFlows = map lowerFlow flows
      let flowMap = Map.fromList [(IR.flowName f, f) | f <- loweredFlows]
      case find (\f -> IR.flowName f == "main") loweredFlows of
        Nothing -> do
          putStrLn "❌ No 'main' flow found."
          return Failed
        Just mainFlow -> do
          putStrLn "✅ Lowered IR:"
          print mainFlow
          putStrLn "🔁 Evaluation Output:"
          result <- try (evalIRFlow flowMap (IR.flowName mainFlow) []) :: IO (Either SomeException (Either EvalError (Maybe Value)))

          case result of
            Left ex -> do
              putStrLn $ "❌ Runtime error: " ++ show ex
              return (if expectedFail then ExpectedFail else Failed)
            Right (Left err) -> do
              putStrLn $ "❌ Runtime error: " ++ show err
              return (if expectedFail then ExpectedFail else Failed)
            Right (Right maybeVal) -> do
              case maybeVal of
                Just v  -> putStrLn $ "✅ Flow returned: " ++ show v
                Nothing -> putStrLn "✅ Flow completed without explicit return"
              return (if expectedFail then Failed else Passed)

summarize :: [TestResult] -> IO ()
summarize results = do
  let passed       = length (filter (== Passed) results)
  let failed       = length (filter (== Failed) results)
  let expectedFail = length (filter (== ExpectedFail) results)
  let total        = length results
  putStrLn $ "✅ Passed: " ++ show passed
  putStrLn $ "❌ Failed: " ++ show failed
  putStrLn $ "⚠️ Expected Failures: " ++ show expectedFail
  putStrLn $ "🧪 Total: " ++ show total
