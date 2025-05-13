-- test/RunAllExamples.hs

module Main where

import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>), takeExtension)
import System.Process (readProcess)
import Control.Monad (filterM, forM_)
import System.Exit (exitFailure)

main :: IO ()
main = do
  putStrLn "🌊 Running all RVRS tests...\n"
  rvrsFiles <- findRVRSFiles "."
  if null rvrsFiles
    then putStrLn "⚠️  No .rvrs files found."
    else forM_ rvrsFiles runAndPrint

findRVRSFiles :: FilePath -> IO [FilePath]
findRVRSFiles path = do
  contents <- listDirectory path
  let fullPaths = map (path </>) contents
  dirs <- filterM doesDirectoryExist fullPaths
  nested <- concat <$> mapM findRVRSFiles dirs
  let files = filter (\f -> takeExtension f == ".rvrs") fullPaths
  return (files ++ nested)

runAndPrint :: FilePath -> IO ()
runAndPrint path = do
  putStrLn $ "🔍 Running: " ++ path
  result <- readProcess "cabal" ["run", "rvrs", path] ""
  putStrLn result
  putStrLn $ replicate 40 '-'
