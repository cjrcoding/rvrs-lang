module Main where

import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>), takeExtension)
import System.Process (readProcessWithExitCode)
import System.Exit (exitFailure)
import Control.Monad (filterM, when)
import Data.List (isInfixOf)
import Data.Char (toLower)

main :: IO ()
main = do
  putStrLn "\x1b[36m🌊 Running all RVRS tests...\x1b[0m\n"
  rvrsFiles <- findRVRSFiles "tests"
  (passes, fails, expectedFails, silentTests) <- runTests rvrsFiles

  putStrLn "\n\x1b[35m🔚 Test Summary:\x1b[0m"
  putStrLn $ "✅ Passed: " ++ show passes
          ++ " | ❌ Failed: " ++ show fails
          ++ " | ⚠️ Expected Failures: " ++ show expectedFails
          ++ " | 🤫 Silent: " ++ show (length silentTests)
          ++ " | 🧪 Total: " ++ show (passes + fails + expectedFails)

  when (not (null silentTests)) $ do
    putStrLn "\n\x1b[90m🤫 Silent tests (no echo/mouth/assert):\x1b[0m"
    mapM_ (putStrLn . (" - " ++)) silentTests

findRVRSFiles :: FilePath -> IO [FilePath]
findRVRSFiles path = do
  contents <- listDirectory path
  let fullPaths = map (path </>) contents
  dirs <- filterM doesDirectoryExist fullPaths
  nested <- concat <$> mapM findRVRSFiles dirs
  let files = filter (\f -> takeExtension f == ".rvrs") fullPaths
  return (files ++ nested)

runTests :: [FilePath] -> IO (Int, Int, Int, [FilePath])
runTests files = go files 0 0 0 []
  where
    go [] p f e s = return (p, f, e, s)
    go (file:rest) p f e s = do
      isExpectedFail <- checkExpectedFail file
      putStrLn $ "\x1b[33m🔍 Running: " ++ file ++ if isExpectedFail then " ⚠️ (expected fail)" else "" ++ "\x1b[0m"
      (exitCode, stdout, stderr) <- readProcessWithExitCode "cabal" ["run", "rvrs", file] ""
      putStrLn stdout
      let combinedOutput = map toLower (stdout ++ stderr)
          failed = any (`isInfixOf` combinedOutput)
            [ "❌ parse failed"
            , "parse failed"
            , "unexpected"
            , "runtime error"
            , "assertion failed"
            , "assertion error"
            , "error:"
            , "could not evaluate"
            ]
          silent = not (any (`isInfixOf` combinedOutput)
            [ "echo:"
            , "mouth:"
            , "assert"
            ])

      when silent $
        putStrLn $ "\x1b[90m🤫 Warning: Silent test — " ++ file ++ "\x1b[0m"

      putStrLn $ replicate 40 '-'

      if failed && isExpectedFail
        then go rest p f (e + 1) (if silent then file:s else s)
      else if failed
        then go rest p (f + 1) e (if silent then file:s else s)
      else go rest (p + 1) f e (if silent then file:s else s)

checkExpectedFail :: FilePath -> IO Bool
checkExpectedFail file = do
  contents <- readFile file
  return $ any ("-- expect-fail" `isInfixOf`) (lines contents)
