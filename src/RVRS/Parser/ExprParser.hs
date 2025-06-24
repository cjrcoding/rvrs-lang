-- src/RVRS/Parser/Expression.hs

module RVRS.Parser.ExprParser (exprParser) where

import Prelude
import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Ya (Recursive (..), type AR, type AR_, type AR__, type Unit, pattern Unit, yi, ho'ho, ha, lu)

import RVRS.AST

type Parser = Parsec Void String

-- Expression entry point
exprParser :: Parser (Recursive Expression)
exprParser = makeExprParser term operatorTable

-- Operator precedence table
operatorTable :: [[Operator Parser (Recursive Expression)]]
operatorTable =
  [ [ InfixL $ binop (Arithmetic `ha` Mul) <$ symbol "*"
    , InfixL $ binop (Arithmetic `ha` Div) <$ symbol "/"
    ]
  , [ InfixL $ binop (Arithmetic `ha` Add) <$ symbol "+"
    , InfixL $ binop (Arithmetic `ha` Sub) <$ symbol "-"
    ]
  , [ InfixL $ binop (Comparison `ha` Equals) <$ symbol "=="
    , InfixL $ binop (Comparison `ha` Greater) <$ symbol "<"
    , InfixL $ binop (Comparison `ha` Less) <$ symbol "<"
    ]
  , [ InfixL $ binop (Combinated `ha` And) <$ symbol "and"
    , InfixL $ binop (Combinated `ha` Or) <$ symbol "or"
    ]
  ]

binop f x y = x `lu` y `lu` f Unit
 `yi` Recursive `ha` Operator `ha` Binary

-- Terms in the expression grammar
term :: Parser (Recursive Expression)
term = do try $ funcCallExpr
   <|> do try $ Recursive (Literal `ha` Bool $ True) <$ symbol "truth"
   <|> do try $ Recursive (Literal . Bool $ False) <$ symbol "void"
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
funcCallExpr = Calling `ho'ho` Recursive <$> identifier <*> parens (exprParser `sepBy` symbol ",")

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
