module Main where

import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>), takeExtension)
import System.Process (readProcessWithExitCode)
import System.Exit (exitFailure)
import Control.Monad (filterM)
import Data.List (isInfixOf)
import Data.Char (toLower)

main :: IO ()
main = do
  putStrLn "\x1b[36m[RVRS] Running all tests...\x1b[0m\n"
  rvrsFiles <- findRVRSFiles "tests"
  (passes, fails, expectedFails, silentTests) <- runTests rvrsFiles []

  putStrLn "\n\x1b[35m[Test Summary]\x1b[0m"
  putStrLn $ "[PASS] Passed: " ++ show passes ++
             " | [FAIL] Failed: " ++ show fails ++
             " | [WARN] Expected Failures: " ++ show expectedFails ++
             " | [TOTAL] Total: " ++ show (passes + fails + expectedFails)

  putStrLn "\n[Silent Tests]"
  mapM_ putStrLn silentTests

findRVRSFiles :: FilePath -> IO [FilePath]
findRVRSFiles path = do
  contents <- listDirectory path
  let fullPaths = map (path </>) contents
  dirs <- filterM doesDirectoryExist fullPaths
  nested <- concat <$> mapM findRVRSFiles dirs
  let files = filter (\f -> takeExtension f == ".rvrs") fullPaths
  return (files ++ nested)

runTests :: [FilePath] -> [FilePath] -> IO (Int, Int, Int, [FilePath])
runTests files silent = go files 0 0 0 silent
  where
    go [] p f e s = return (p, f, e, reverse s)
    go (file:rest) p f e s = do
      isExpectedFail <- checkExpectedFail file
      putStrLn $ "[RUN] " ++ file ++ if isExpectedFail then " [expected fail]" else ""
      (exitCode, stdout, stderr) <- readProcessWithExitCode "cabal" ["run", "rvrs", file] ""
      putStrLn stdout
      let combinedOutput = map toLower (stdout ++ stderr)
          failed = any (`isInfixOf` combinedOutput)
            [ "parse failed"
            , "unexpected"
            , "runtime error"
            , "assertion failed"
            , "assertion error"
            , "error:"
            , "could not evaluate"
            ]
          isSilent = not ("echo:" `isInfixOf` combinedOutput ||
                          "returned:" `isInfixOf` combinedOutput ||
                          "whisper:" `isInfixOf` combinedOutput ||
                          "eval error:" `isInfixOf` combinedOutput ||
                          "mouth:" `isInfixOf` combinedOutput)

      putStrLn $ replicate 40 '-'
      let s' = if isSilent then file : s else s

      if failed && isExpectedFail
        then go rest p f (e + 1) s'
      else if failed
        then go rest p (f + 1) e s'
      else go rest (p + 1) f e s'

checkExpectedFail :: FilePath -> IO Bool
checkExpectedFail file = do
  contents <- readFile file
  return $ any ("-- expect-fail" `isInfixOf`) (lines contents)
