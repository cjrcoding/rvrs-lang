module RVRS.Parser.ExprParser (exprParser, identifier) where

import Prelude
import GHC.IsList (fromList)
import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Void
import Data.String
import Text.Megaparsec
import Text.Megaparsec.Char

import qualified Text.Megaparsec.Char.Lexer as M
import qualified Text.Megaparsec as M
import qualified Control.Monad.Combinators.Expr as M

import Ya (Recursive (..), Object (..), type AR, type AR_, type AR__, type Unit, pattern Both, pattern Only, pattern Unit, by, yi, ho, ho'ho, ha, hv, lu, wrap)

import RVRS.AST

type Parser = Parsec Void String

-- Expression entry point
exprParser :: Parser (Recursive Expression)
exprParser = makeExprParser term operatorTable

-- Operator precedence table
operatorTable :: [[M.Operator Parser (Recursive Expression)]]
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

binop f x y = Operation `hv` f Unit `hv` Both (x `lu` y)
 `yi` Recursive `ha` Operator `ha` Dyadic

-- Terms in the expression grammar
term :: Parser (Recursive Expression)
term = do try $ funcCallExpr
   <|> do try $ Recursive (Operand `ha` Literal `ha` Bool $ True) <$ symbol "truth"
   <|> do try $ Recursive (Operand `ha` Literal `ha` Bool $ False) <$ symbol "void"
   <|> do try $ Recursive `ha` Operand `ha` Literal `ha` String <$> stringLiteral
   <|> do try $ parseNumber
   <|> do try $ Recursive . Operator . Unary . Operation (by Complement) . Only <$> (symbol "not" *> term)
   <|> do try $ Recursive . Operator . Unary . Operation (by Negation) . Only <$> (symbol "-" *> term)
   <|> do try $ parens exprParser
   <|> Recursive . Operand . Variable <$> identifier

-- Parse numeric literals
parseNumber :: Parser (Recursive Expression)
parseNumber = Recursive . Operand . Literal <$> Double <$> do lexeme $ try M.float <|> fromInteger <$> M.decimal

-- Function call expressions (e.g., fuse(2, 3))
funcCallExpr :: Parser (Recursive Expression)
funcCallExpr = Calling `ho` Recursive
 <$> (These <$> (wrap <$> identifier) <*> (fromList <$> parens (exprParser `sepBy` symbol ",")))

-- Utility parsers
lexeme :: Parser a -> Parser a
lexeme = M.lexeme sc

symbol :: String -> Parser String
symbol = M.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

identifier :: Parser Name
identifier = fromString <$> do lexeme $ (:) <$> lowerChar <*> many letterChar

stringLiteral :: Parser String
stringLiteral = char '"' *> manyTill M.charLiteral (char '"')

sc :: Parser ()
sc = M.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = M.skipLineComment "--"
    blockCmnt = M.skipBlockComment "{-" "-}"
