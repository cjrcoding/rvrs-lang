module RVRS.Parser (parseRVRS) where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import GHC.IsList (fromList, toList)
import Data.Char (isAlphaNum)
import Data.Void
import Debug.Trace (trace)

import RVRS.AST
import RVRS.Parser.StmtParser (statementParser)

type Parser = Parsec Void String

-- 🔧 Debug toggle
debug :: Bool
debug = False  -- Set to True if you want parser debug output

-- Top-level parse function
parseRVRS :: String -> Either (ParseErrorBundle String Void) [Flow]
parseRVRS input =
  case parse (between sc eof (many flowParser)) "RVRS" input of
    Left err -> trace "❌ PARSE FAILED" (Left err)
    Right flows ->
      if debug
        then trace ("✅ Parsed flows:\n" ++ show flows) (Right flows)
        else Right flows


flowParser :: Parser Flow
flowParser = Flow
  <$> ((symbol "flow" <|> symbol "ceremony") *> identifier)
  <*> argListParser
  <*> (fromList <$> between (symbol "{") (symbol "}") (many (sc *> statementParser <* sc)))

-- Optional argument list
argListParser :: Parser [Argument]
argListParser =
  option [] $ between (symbol "(") (symbol ")") (nameArg `sepBy` symbol ",")
  where
    nameArg = Argument <$> identifier <*> pure "Unknown"

-- Shared utilities
sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "--"
    blockCmnt = L.skipBlockComment "{-" "-}"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

identifier :: Parser String
identifier = lexeme $ (:) <$> letterChar <*> many (alphaNumChar <|> char '_')
