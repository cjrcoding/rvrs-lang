-- src/RVRS/Parser.hs

module RVRS.Parser where

import Debug.Trace (trace)
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

parseRVRS :: String -> Either (ParseErrorBundle String Void) [Flow]
parseRVRS input =
  case parse (between sc eof (many flowParser)) "RVRS" input of
    Left err -> trace "❌ PARSE FAILED" (Left err)
    Right flows -> trace ("✅ Parsed flows:\n" ++ show flows) (Right flows)



argumentParser :: Parser Argument
argumentParser = do
  name <- identifier
  _ <- symbol ":"
  typ <- identifier
  return $ Argument name typ

argListParser :: Parser [Argument]
argListParser =
  between (symbol "(") (symbol ")") (argumentParser `sepBy` symbol ",")

-- ✅ UPDATED: uses `many (sc *> statementParser)` for clean flow body parsing
flowParser :: Parser Flow
flowParser = do
  _ <- symbol "flow"
  name <- identifier
  args <- argListParser
  body <- between (symbol "{") (symbol "}") (many (sc *> statementParser))
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

-- ✅ UPDATED: callStmt now wrapped in `try` to prevent parse commit errors
statementParser :: Parser Statement
statementParser = lexeme $
      try pillarParser
  <|> try mouthParser
  <|> try echoParser
  <|> try sourceParser
  <|> try deltaParser
  <|> try branchParser
  <|> try returnParser
  <|> try callStmt

mouthParser :: Parser Statement
mouthParser = do
  symbol "mouth"
  expr <- exprParser  
  return $ Mouth expr

echoParser :: Parser Statement
echoParser = do
  symbol "echo"
  expr <- exprParser  
  return $ Echo expr

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
  symbol "branch"
  cond <- exprParser
  thenBlock <- blockParser
  elseBlock <- optionalElseParser
  return $ Branch cond thenBlock elseBlock

optionalElseParser :: Parser [Statement]
optionalElseParser =
      (symbol "else" *> blockParser)
  <|> pure []

exprParser :: Parser Expr
exprParser = makeExprParser term operatorTable

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ InfixL (Mul <$ symbol "*")
    , InfixL (Div <$ symbol "/")
    ]
  , [ InfixL (Add <$ symbol "+")
    , InfixL (Sub <$ symbol "-")
    ]
  , [ InfixL (Equals <$ symbol "==") ]
  , [ InfixL (And <$ symbol "and")
    , InfixL (Or <$ symbol "or")
    ]
  ]

-- ✅ CLEANED: callExprParser only appears once, at top
term :: Parser Expr
term =
      try callExprParser
  <|> try (BoolLit True <$ symbol "truth")
  <|> try (BoolLit False <$ symbol "void")
  <|> try (StrLit <$> stringLiteral)
  <|> try parseNumber
  <|> try (Not <$> (symbol "not" *> term))
  <|> try (between (symbol "(") (symbol ")") exprParser)
  <|> Var <$> identifier


parseNumber :: Parser Expr
parseNumber = do
  num <- lexeme $ try L.float <|> (fromInteger <$> L.decimal)
  return $ NumLit num


callExprParser :: Parser Expr
callExprParser = do
  _ <- symbol "call"
  name <- identifier
  _ <- symbol "(" *> symbol ")"
  return $ CallExpr name

pillarParser :: Parser Statement
pillarParser = do
  _ <- symbol "pillar"
  var <- identifier
  _ <- symbol "="
  expr <- exprParser
  return $ Pillar var expr

returnParser :: Parser Statement
returnParser = do
  _ <- symbol "return"
  expr <- exprParser
  return $ Return expr

blockParser :: Parser [Statement]
blockParser = do
  symbol "{" >> many statementParser <* symbol "}"

callStmt :: Parser Statement
callStmt = do
  _ <- symbol "call"
  name <- identifier
  return $ Call name
