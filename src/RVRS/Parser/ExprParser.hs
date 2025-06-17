-- src/RVRS/Parser/Expression.hs

module RVRS.Parser.ExprParser (exprParser) where

import Ya (Recursive (..))
import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import RVRS.AST
import RVRS.Utils

type Parser = Parsec Void String

-- Expression entry point
exprParser :: Parser (Recursive Expression)
exprParser = makeExprParser term operatorTable

-- Operator precedence table
operatorTable :: [[Operator Parser (Recursive Expression)]]
operatorTable =
  [ [ InfixL (Recursive . Operator . Binary <$$> Mul <$ symbol "*")
    , InfixL (Recursive . Operator . Binary <$$> Div <$ symbol "/")
    ]
  , [ InfixL (Recursive . Operator . Binary <$$> Add <$ symbol "+")
    , InfixL (Recursive . Operator . Binary <$$> Sub <$ symbol "-")
    ]
  , [ InfixN (Recursive . Operator . Binary <$$> Equals      <$ symbol "==")
    , InfixN (Recursive . Operator . Binary <$$> Greater <$ symbol ">")
    , InfixN (Recursive . Operator . Binary <$$> Less    <$ symbol "<")
    ]
  , [ InfixL ((Recursive . Operator . Binary <$$> And) <$ symbol "and")
    , InfixL ((Recursive . Operator . Binary <$$> Or)  <$ symbol "or")
    ]
  ]

-- Terms in the expression grammar
term :: Parser (Recursive Expression)
term = do try $ funcCallExpr
   <|> do try $ Recursive (Literal $ Bool True) <$ symbol "truth"
   <|> do try $ Recursive (Literal $ Bool False) <$ symbol "void"
   <|> do try $ Recursive . Literal . String <$> stringLiteral
   <|> do try $ parseNumber
   <|> do try $ Recursive . Operator . Unary . Not <$> (symbol "not" *> term)
   <|> do try $ Recursive . Operator . Unary . Neg <$> (symbol "-" *> term)
   <|> do try $ parens exprParser
   <|> Recursive . Variable <$> identifier

-- Parse numeric literals
parseNumber :: Parser (Recursive Expression)
parseNumber = Recursive . Literal <$> Double <$> do lexeme $ try L.float <|> fromInteger <$> L.decimal

-- Function call expressions (e.g., fuse(2, 3))
funcCallExpr :: Parser (Recursive Expression)
funcCallExpr = Recursive <$$> Calling <$> identifier <*> parens (exprParser `sepBy` symbol ",")

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
