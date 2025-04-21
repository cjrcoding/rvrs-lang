-- src/RVRS/Parser.hs

module RVRS.Parser where

import RVRS.AST
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Text (Text)
import Control.Monad (void)
import Data.Char (isAlphaNum)

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
-- Now supports flow body with statement parsing
flowParser :: Parser Flow
flowParser = do
  _ <- symbol "flow"
  name <- identifier
  args <- argListParser
  body <- between (symbol "{") (symbol "}") (many (lexeme statementParser))
  return $ Flow name args body

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
identifier = lexeme $ (:) <$> letterChar <*> many (alphaNumChar <|> char '_')

-- | Parses string like "hello"
stringLiteral :: Parser String
stringLiteral = char '"' *> manyTill L.charLiteral (char '"')

-- | Statement parser dispatch

statementParser =
      try mouthParser
  <|> try echoParser
  <|> try sourceParser
  <|> try deltaParser
  <|> try branchParser

-- | mouth "hello"
mouthParser :: Parser Statement
mouthParser = do
  _ <- symbol "mouth"
  str <- stringLiteral
  return $ Mouth (StrLit str)

-- | echo "goodbye"
echoParser :: Parser Statement
echoParser = do
  _ <- symbol "echo"
  str <- stringLiteral
  return $ Echo (StrLit str)

sourceParser :: Parser Statement
sourceParser = do
  _ <- symbol "source"
  var <- identifier
  _ <- symbol "="
  expr <- exprParser
  return $ Source var expr

deltaParser :: Parser Statement
deltaParser = do
  _ <- symbol "delta"
  var <- identifier
  _ <- symbol "="
  expr <- exprParser
  return $ Delta var expr

-- | Expression parser with support for function calls, string/bool literals, and equality
exprParser :: Parser Expr
exprParser = makeExprParser term operatorTable

-- | Term is a single literal, variable, or function call
term :: Parser Expr
term =
      try (BoolLit True <$ symbol "true")
  <|> try (BoolLit False <$ symbol "false")
  <|> try (StrLit <$> stringLiteral)
  <|> try callParser
  <|> Var <$> identifier

-- | Operator precedence table (can be expanded later)
operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ InfixL (Equals <$ symbol "==") ]
  ]
  
callParser :: Parser Expr
callParser = do
  func <- identifier
  args <- between (symbol "(") (symbol ")") (exprParser `sepBy` symbol ",")
  return $ Call func args

branchParser :: Parser Statement
branchParser = do
  _ <- symbol "branch"
  cond <- exprParser
  ifBlock <- between (symbol "{") (symbol "}") (many (lexeme statementParser))
  _ <- symbol "else"
  elseBlock <- between (symbol "{") (symbol "}") (many (lexeme statementParser))
  return $ Branch cond ifBlock elseBlock


