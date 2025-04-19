-- src/RVRS/Parser.hs

module RVRS.Parser where

import RVRS.AST
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Text (Text)

-- | Parser type
-- Reminder: 'Void' is for no custom errors, 'String' is what I'm parsing
-- This is the base type all my parsers build on
-- Eventually I could swap 'String' for 'Text', but this works for now
type Parser = Parsec Void String

-- | Entry point — this is what runs when I call parseRVRS on file input
-- If I get a Left, it's a parse error; Right gives me a Flow object
parseRVRS :: String -> Either (ParseErrorBundle String Void) Flow
parseRVRS input = parse flowParser "RVRS" input

-- | Single argument like: user: Identity
-- Grabs name, skips the colon, grabs type
-- Feeds into my Flow's flowArgs as Argument name type
argumentParser :: Parser Argument
argumentParser = do
  name <- identifier
  _ <- symbol ":"
  typ <- identifier
  return $ Argument name typ

-- | Parses (arg1: Type, arg2: Type)
-- Between ( and ), splits by commas, uses argumentParser above
argListParser :: Parser [Argument]
argListParser =
  between (symbol "(") (symbol ")") (argumentParser `sepBy` symbol ",")

-- | The core — parses a full flow declaration header
-- For now: flow name(args), body is empty
-- Next step: add `{}` block parsing
flowParser :: Parser Flow
flowParser = do
  _ <- symbol "flow"
  name <- identifier
  args <- argListParser
  return $ Flow name args []

-- | Whitespace + comment skipping
-- Keeps things clean — supports -- and {- -} style comments
sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "--"
    blockCmnt = L.skipBlockComment "{-" "-}"

-- | Utility wrappers
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

identifier :: Parser String
-- Letters followed by optional alphanum string (e.g. grant_access, user1)
identifier = lexeme ((:) <$> letterChar <*> many alphaNumChar)

-- | Parses string like "hello"
stringLiteral :: Parser String
stringLiteral = char '"' *> manyTill L.charLiteral (char '"')
