-- RunEngine.hs
module Main where

import RVRS.Parser (parseRVRS)
import RVRS.Engine (flowing)
import RVRS.AST (Flow)
import Ya (Object(..))  
import qualified Data.Text.IO as T
import qualified Data.Text as Text
import System.Directory (listDirectory)
import System.FilePath ((</>), takeExtension)
import Data.Map (fromList)
import Control.Monad (forM_)
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = do
  let testDir = "tests/engine"
  files <- listDirectory testDir
  let rvrsFiles = filter (\f -> takeExtension f == ".rvrs") files
  putStrLn $ "[ENGINE TESTS] Running " ++ show (length rvrsFiles) ++ " test(s)..."
  forM_ rvrsFiles $ \file -> runTestFile (testDir </> file)

runTestFile :: FilePath -> IO ()
runTestFile path = do
  putStrLn $ "\n[RUN] " ++ path
  src <- T.readFile path
  case parseRVRS (Text.unpack src) of
    Left err -> do
      putStrLn "[Parse Error]"
      putStrLn $ errorBundlePretty err
    Right flows -> do
      let flowMap = fromList [(name, flow) | These flow name <- flows]
      result <- flowing flowMap "main" []
      putStrLn $ "Result: " -- ++ show result
