-- src/RVRS/Parser.hs

module RVRS.Parser where

import RVRS.AST
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Text (Text)

-- | Parser type
-- 'Void' means we don't care about custom error types for now
-- 'String' is our input type (source code lines)
type Parser = Parsec Void String

-- | Entry point for parsing a full flow
parseRVRS :: String -> Either (ParseErrorBundle String Void) Flow
parseRVRS input = parse flowParser "RVRS" input

-- | Step 1: Parse just 'flow' and the function name
flowParser :: Parser Flow
flowParser = do
  _ <- symbol "flow"
  name <- identifier
  _ <- symbol "("
  _ <- symbol ")"
  return $ Flow name [] []

-- | Utilities for parsing
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
identifier = lexeme ((:) <$> letterChar <*> many alphaNumChar)

stringLiteral :: Parser String
stringLiteral = char '"' *> manyTill L.charLiteral (char '"')
