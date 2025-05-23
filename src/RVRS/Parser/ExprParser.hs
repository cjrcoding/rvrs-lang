-- src/RVRS/Parser/Expression.hs

module RVRS.Parser.ExprParser (exprParser) where

-- Internal modules
import RVRS.AST

-- External libraries
import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void String

-- Expression entry point
exprParser :: Parser Expr
exprParser = makeExprParser term operatorTable

-- Operator precedence table
operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ InfixL (Mul <$ symbol "*")
    , InfixL (Div <$ symbol "/")
    ]
  , [ InfixL (Add <$ symbol "+")
    , InfixL (Sub <$ symbol "-")
    ]
  , [ InfixN (Equals      <$ symbol "==")
    , InfixN (GreaterThan <$ symbol ">")
    , InfixN (LessThan    <$ symbol "<")
    ]
  , [ InfixL (And <$ symbol "and")
    , InfixL (Or  <$ symbol "or")
    ]
  ]

-- Terms in the expression grammar
term :: Parser Expr
term =
      try funcCallExpr
  <|> try (BoolLit True  <$ symbol "truth")
  <|> try (BoolLit False <$ symbol "void")
  <|> try (StrLit <$> stringLiteral)
  <|> try parseNumber
  <|> try (Not <$> (symbol "not" *> term))
  <|> try (Neg <$> (symbol "-" *> term))
  <|> try (parens exprParser)
  <|> Var <$> identifier

-- Parse numeric literals
parseNumber :: Parser Expr
parseNumber = do
  num <- lexeme $ try L.float <|> (fromInteger <$> L.decimal)
  return $ NumLit num

-- Function call expressions (e.g., fuse(2, 3))
funcCallExpr :: Parser Expr
funcCallExpr = do
  funcName <- identifier
  args <- parens (exprParser `sepBy` symbol ",")
  return $ CallExpr funcName args

-- Utility parsers
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

identifier :: Parser String
identifier = lexeme $ (:) <$> letterChar <*> many (alphaNumChar <|> char '_')

stringLiteral :: Parser String
stringLiteral = char '"' *> manyTill L.charLiteral (char '"')

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "--"
    blockCmnt = L.skipBlockComment "{-" "-}"
