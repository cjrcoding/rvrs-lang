-- tests/RunAll.hs

module Main where

import System.Directory (doesDirectoryExist, listDirectory, doesFileExist)
import System.FilePath ((</>), takeExtension)
import System.Process (readProcess)
import Control.Monad (filterM, forM)
import Data.List (isInfixOf)
import Text.Printf (printf)

main :: IO ()
main = do
  putStrLn "🌊 Running all RVRS tests...\n"
  rvrsFiles <- findRVRSFiles "tests"

  if null rvrsFiles
    then putStrLn "⚠️  No .rvrs files found."
    else do
      results <- mapM runAndPrint rvrsFiles

      let passed = length (filter id results)
          total  = length results
          failed = total - passed

      putStrLn "\n🔚 Test Summary:"
      printf "✅ Passed: %d | ❌ Failed: %d | 🧪 Total: %d\n" passed failed total

-- Recursively find all .rvrs files under a directory
findRVRSFiles :: FilePath -> IO [FilePath]
findRVRSFiles path = do
  contents <- listDirectory path
  let fullPaths = map (path </>) contents
  dirs  <- filterM doesDirectoryExist fullPaths
  files <- filterM doesFileExist fullPaths
  let rvrsFiles = filter (\f -> takeExtension f == ".rvrs") files
  nested <- concat <$> mapM findRVRSFiles dirs
  return (rvrsFiles ++ nested)

-- Run and print result; return True if passed, False if failed
runAndPrint :: FilePath -> IO Bool
runAndPrint path = do
  putStrLn $ "🔍 Running: " ++ path
  output <- readProcess "cabal" ["run", "rvrs", path] ""
  putStrLn output
  putStrLn $ replicate 40 '-'

  let isFail = any (`isInfixOf` output)
        [ "❌ PARSE FAILED"
        , "Error:"
        , "Assertion failed"
        , "Assertion error"
        , "Could not evaluate"
        ]

  return (not isFail)
