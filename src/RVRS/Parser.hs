
-- src/RVRS/Parser.hs

module RVRS.Parser where

import RVRS.AST
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Text (Text)
import Control.Monad (void)
import Data.Char (isAlphaNum)
import Control.Monad.Combinators.Expr


type Parser = Parsec Void String


parseRVRS :: String -> Either (ParseErrorBundle String Void) Flow
parseRVRS input = parse flowParser "RVRS" input


argumentParser :: Parser Argument
argumentParser = do
  name <- identifier
  _ <- symbol ":"
  typ <- identifier
  return $ Argument name typ


argListParser :: Parser [Argument]
argListParser =
  between (symbol "(") (symbol ")") (argumentParser `sepBy` symbol ",")


flowParser :: Parser Flow
flowParser = do
  _ <- symbol "flow"
  name <- identifier
  args <- argListParser
  body <- between (symbol "{") (symbol "}") (many (lexeme statementParser))
  return $ Flow name args body


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


stringLiteral :: Parser String
stringLiteral = char '"' *> manyTill L.charLiteral (char '"')


statementParser =
      try mouthParser
  <|> try echoParser
  <|> try sourceParser
  <|> try deltaParser
  <|> try branchParser
  <|> try pillarParser


mouthParser :: Parser Statement
mouthParser = do
  _ <- symbol "mouth"
  str <- stringLiteral
  return $ Mouth (StrLit str)


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

branchParser :: Parser Statement
branchParser = do
  _ <- symbol "branch"
  cond <- between (symbol "(") (symbol ")") exprParser
  trueBranch <- between (symbol "{") (symbol "}") (many (lexeme statementParser))
  _ <- symbol "else"
  falseBranch <- between (symbol "{") (symbol "}") (many (lexeme statementParser))
  return $ Branch cond trueBranch falseBranch


exprParser :: Parser Expr
exprParser = makeExprParser term operatorTable

term :: Parser Expr
term =
      try (BoolLit True <$ (symbol "truth" <|> symbol "true"))
  <|> try (BoolLit False <$ (symbol "void" <|> symbol "false"))
  <|> try (StrLit <$> stringLiteral)
  <|> try callParser
  <|> Var <$> identifier

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ InfixL (Equals <$ symbol "==") ]
  ]

callParser :: Parser Expr
callParser = do
  func <- identifier
  args <- between (symbol "(") (symbol ")") (exprParser `sepBy` symbol ",")
  return $ Call func args

pillarParser :: Parser Statement
pillarParser = do
  _ <- symbol "pillar"
  var <- identifier
  _ <- symbol "="
  expr <- exprParser
  return $ Pillar var expr

