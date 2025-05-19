module Main where

import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>), takeExtension)
import System.Process (readProcess)
import Control.Monad (filterM, forM_)
import System.Exit (exitFailure)
import Data.List (isInfixOf)

main :: IO ()
main = do
  putStrLn "\x1b[36m🌊 Running all RVRS tests...\x1b[0m\n"
  rvrsFiles <- findRVRSFiles "tests"
  (passes, fails, expectedFails) <- runTests rvrsFiles
  putStrLn "\n\x1b[35m🔚 Test Summary:\x1b[0m"
  putStrLn $ "✅ Passed: " ++ show passes ++
             " | ❌ Failed: " ++ show fails ++
             " | ⚠️ Expected Failures: " ++ show expectedFails ++
             " | 🧪 Total: " ++ show (passes + fails + expectedFails)

findRVRSFiles :: FilePath -> IO [FilePath]
findRVRSFiles path = do
  contents <- listDirectory path
  let fullPaths = map (path </>) contents
  dirs <- filterM doesDirectoryExist fullPaths
  nested <- concat <$> mapM findRVRSFiles dirs
  let files = filter (\f -> takeExtension f == ".rvrs") fullPaths
  return (files ++ nested)

runTests :: [FilePath] -> IO (Int, Int, Int)
runTests files = go files 0 0 0
  where
    go [] p f e = return (p, f, e)
    go (file:rest) p f e = do
      isExpectedFail <- checkExpectedFail file
      putStrLn $ "\x1b[33m🔍 Running: " ++ file ++ if isExpectedFail then " ⚠️ (expected fail)" else "" ++ "\x1b[0m"
      output <- readProcess "cabal" ["run", "rvrs", file] ""
      putStrLn output
      putStrLn $ replicate 40 '-'

      let failed = any (`isInfixOf` output)
            ["PARSE FAILED", "Error:", "Assertion failed:", "Assertion error:"]

      if failed && isExpectedFail
        then go rest p f (e + 1)
      else if failed
        then go rest p (f + 1) e
      else go rest (p + 1) f e

checkExpectedFail :: FilePath -> IO Bool
checkExpectedFail file = do
  contents <- readFile file
  return $ any ("-- expect-fail" `isInfixOf`) (lines contents)
