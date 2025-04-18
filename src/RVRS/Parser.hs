module RVRS.Parser where

import RVRS.AST
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Text (Text)

-- | Parser type
type Parser = Parsec Void String

-- | Entry point for parsing a full flow
parseRVRS :: String -> Either (ParseErrorBundle String Void) Flow
parseRVRS input = parse flowParser "RVRS" input

-- | Basic flow parser — placeholder, to be expanded
flowParser :: Parser Flow
flowParser = do
  -- We'll eventually parse: flow name(args) { body }
  fail "flowParser not implemented yet"

-- | Utilities for future parsing
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
