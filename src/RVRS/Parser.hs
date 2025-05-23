module RVRS.Parser (parseRVRS) where

-- Internal: RVRS language components
import RVRS.AST
import RVRS.Parser.StmtParser (statementParser)
import RVRS.Parser.Type (RVRSType(..), parseType)

-- External: Parsing libraries
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- External: General utilities
import Data.Char (isAlphaNum)
import Data.Void (Void)

-- Debug (optional, for tracing parse failures)
import Debug.Trace (trace)

type Parser = Parsec Void String

-- 🔧 Debug toggle
debug :: Bool
debug = False

-- Top-level parse function
parseRVRS :: String -> Either (ParseErrorBundle String Void) [Flow]
parseRVRS input =
  case parse (between sc eof (many flowParser)) "RVRS" input of
    Left err -> trace "❌ PARSE FAILED" (Left err)
    Right flows ->
      if debug
        then trace ("✅ Parsed flows:\n" ++ show flows) (Right flows)
        else Right flows

-- 🔁 Flow parser (supports both old + new styles)
flowParser :: Parser Flow
flowParser = try flowParserNew <|> flowParserLegacy

-- 🆕 New-style flow:
-- flow greet name: Str, age: Num returns Str { ... }
flowParserNew :: Parser Flow
flowParserNew = do
  _ <- symbol "flow" <|> symbol "ceremony"
  name <- identifier
  args <- typedArgParser `sepBy` symbol ","
  rtype <- optional (symbol "returns" *> parseType)
  body <- between (symbol "{") (symbol "}") (many (sc *> statementParser <* sc))
  return $ Flow name args body

-- 🧓 Legacy-style flow:
-- flow greet(name) { ... }
flowParserLegacy :: Parser Flow
flowParserLegacy = do
  _ <- symbol "flow" <|> symbol "ceremony"
  name <- identifier
  args <- argListParser
  body <- between (symbol "{") (symbol "}") (many (sc *> statementParser <* sc))
  return $ Flow name args body

-- Old-style arguments: (x, y)
argListParser :: Parser [Argument]
argListParser =
  option [] $ between (symbol "(") (symbol ")") (nameArg `sepBy` symbol ",")
  where
    nameArg = Argument <$> identifier <*> pure TypeAny

-- New-style typed arg: x: Num
typedArgParser :: Parser Argument
typedArgParser = do
  name <- identifier
  _ <- symbol ":"
  ty <- parseType
  return $ Argument name ty

-- Shared whitespace / token parsers
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
