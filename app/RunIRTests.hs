module Main where

-- Internal modules
import RVRS.Parser (parseRVRS)
import RVRS.Lower (lowerFlow)
import RVRS.Eval (evalIRFlow, EvalError)
import qualified RVRS.AST as AST

-- System / standard libraries
import System.Directory (listDirectory)
import System.FilePath ((</>), takeExtension)
import Data.List (isInfixOf, isPrefixOf, find)
import Control.Monad (forM)
import Control.Exception (try, SomeException)
import qualified Data.Map as Map

testsDir :: FilePath
testsDir = "tests/ir"

data TestResult
  = Passed
  | Failed
  | ExpectedFailCorrect
  | ExpectedFailMismatch
  deriving (Eq)

main :: IO ()
main = do
  putStrLn "[IR TEST] Running all IR tests..."
  files <- listDirectory testsDir
  let rvrsFiles = filter (\f -> takeExtension f == ".rvrs") files

  results <- forM rvrsFiles runIRTest
  putStrLn "\n[Test Summary]"
  summarize results

runIRTest :: FilePath -> IO TestResult
runIRTest file = do
  let path = testsDir </> file
  putStrLn $ "\n[RUN] " ++ path
  content <- readFile path
  let expectedFail = any ("-- expect-fail" `isPrefixOf`) (lines content)

  case parseRVRS content of
    Left err -> do
      putStrLn $ "[FAIL] Parse error:\n" ++ show err
      return (if expectedFail then ExpectedFailCorrect else Failed)
    Right flows -> do
      let loweredFlows = map lowerFlow flows
      let flowMap = Map.fromList [(AST.flowNameIR f, f) | f <- loweredFlows]
      case lookupMain loweredFlows of
        Nothing -> do
          putStrLn "[FAIL] No 'main' flow found."
          return (if expectedFail then ExpectedFailCorrect else Failed)
        Just mainFlow -> do
          putStrLn "[INFO] Lowered IR:"
          print mainFlow
          putStrLn "[INFO] Evaluation Output:"
          result <- try (evalIRFlow flowMap (AST.flowNameIR mainFlow) []) :: IO (Either SomeException (Either EvalError (Maybe AST.Value)))

          case result of
            Left ex -> do
              putStrLn $ "[FAIL] Runtime error: " ++ show ex
              return (if expectedFail then ExpectedFailCorrect else Failed)
            Right (Left err) -> do
              putStrLn $ "[FAIL] Runtime error: " ++ show err
              return (if expectedFail then ExpectedFailCorrect else Failed)
            Right (Right maybeVal) -> do
              case maybeVal of
                Just v  -> putStrLn $ "[PASS] Flow returned: " ++ show v
                Nothing -> putStrLn "[PASS] Flow completed without explicit return"
              return (if expectedFail then ExpectedFailMismatch else Passed)

lookupMain :: [AST.FlowIR] -> Maybe AST.FlowIR
lookupMain = find (\f -> AST.flowNameIR f == "main")

summarize :: [TestResult] -> IO ()
summarize results = do
  let passed           = length (filter (== Passed) results)
  let failed           = length (filter (== Failed) results)
  let expectedFails    = length (filter (== ExpectedFailCorrect) results)
  let unexpectedPasses = length (filter (== ExpectedFailMismatch) results)
  let total            = length results

  putStrLn $ "[PASS] Passed: " ++ show passed
  putStrLn $ "[FAIL] Failed: " ++ show failed
  putStrLn $ "[WARN] Expected Failures: " ++ show expectedFails
  putStrLn $ "[MISS] Unexpected Passes (marked fail but passed): " ++ show unexpectedPasses
  putStrLn $ "[TOTAL] Total: " ++ show total
