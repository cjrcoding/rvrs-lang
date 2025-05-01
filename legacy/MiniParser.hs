-- Minimal RVRS parser & test case to isolate flow parsing issues

-- src/MiniParser.hs
module MiniParser (parseRVRS, Flow(..), Statement(..), Expr(..)) where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Control.Monad (void)
import Control.Monad.Combinators.Expr
import qualified Data.Text as T

-- AST types

data Flow = Flow String [Statement] deriving Show

data Statement
  = Echo Expr
  | Source String Expr
  | Delta String Expr
  | Return Expr
  | Call String
  | Branch Expr [Statement] [Statement]
  deriving Show

data Expr
  = Var String
  | NumLit Double
  | BoolLit Bool
  | StrLit String
  | CallExpr String
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  deriving Show

-- Parser setup

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt = L.skipLineComment "--"
    blockCmnt = L.skipBlockComment "{-" "-}"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc
symbol :: String -> Parser String
symbol = L.symbol sc
identifier :: Parser String
identifier = lexeme ((:) <$> letterChar <*> many alphaNumChar)
stringLiteral :: Parser String
stringLiteral = char '"' *> manyTill L.charLiteral (char '"')

-- Flow parser
parseRVRS :: String -> Either (ParseErrorBundle String Void) [Flow]
parseRVRS input = parse (between sc eof (many flowParser)) "RVRS" input

flowParser :: Parser Flow
flowParser = do
  _ <- symbol "flow"
  name <- identifier
  _ <- symbol "(" >> symbol ")"  -- empty arg list
  body <- between (symbol "{") (symbol "}") (many (sc *> statementParser))
  return $ Flow name body

statementParser :: Parser Statement
statementParser = lexeme $ choice
  [ Echo <$> (symbol "echo" *> exprParser)
  , Source <$> (symbol "source" *> identifier) <*> (symbol "=" *> exprParser)
  , Delta <$> (symbol "delta" *> identifier) <*> (symbol "=" *> exprParser)
  , Return <$> (symbol "return" *> exprParser)
  , Call <$> (symbol "call" *> identifier)
  ]

exprParser :: Parser Expr
exprParser = makeExprParser term operatorTable

term :: Parser Expr
term = choice
  [ try callExprParser
  , NumLit <$> lexeme L.float
  , Var <$> identifier
  , StrLit <$> stringLiteral
  , between (symbol "(") (symbol ")") exprParser
  ]

callExprParser :: Parser Expr
callExprParser = do
  _ <- symbol "call"
  name <- identifier
  _ <- symbol "(" *> symbol ")"
  return $ CallExpr name

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [InfixL (Mul <$ symbol "*"), InfixL (Div <$ symbol "/")]
  , [InfixL (Add <$ symbol "+"), InfixL (Sub <$ symbol "-")]
  ]
