import RVRS.Parser (parseRVRS)
import RVRS.Codegen (prettyPrintFlow)
import RVRS.Eval (evalFlow)
import RVRS.AST (Flow)
import System.Environment (getArgs)
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      content <- readFile filename
      case parseRVRS content of
        Left err -> putStrLn $ errorBundlePretty err
        Right ast -> do
          putStrLn "Parsed Flow:\n"
          putStrLn (prettyPrintFlow ast)
          putStrLn "\nEvaluation Output:"
          evalFlow ast
    _ -> putStrLn "Usage: rvrs <file>.rvrs"
