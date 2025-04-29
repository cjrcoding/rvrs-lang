
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


parseRVRS :: String -> Either (ParseErrorBundle String Void) [Flow]
parseRVRS input = parse (many flowParser) "RVRS" input


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
  body <- between (symbol "{") (symbol "}") (many statementParser)
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

statementParser :: Parser Statement
statementParser = lexeme $
      try pillarParser
  <|> try mouthParser
  <|> try echoParser
  <|> try sourceParser
  <|> try deltaParser
  <|> try branchParser
  <|> try returnParser
  <|> callStmt  



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
  <|> pure []  -- no else block provided


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

binary :: String -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ symbol name)

term =
      try (BoolLit True <$ symbol "truth")
  <|> try (BoolLit False <$ symbol "void")
  <|> try (StrLit <$> stringLiteral)
  <|> try (NumLit . read <$> lexeme (some digitChar))
  <|> try (Not <$> (symbol "not" *> term))             
--  <|> try callParser
  <|> try (between (symbol "(") (symbol ")") exprParser)
  <|> Var <$> identifier



-- callParser :: Parser Expr
-- callParser = do
--  func <- identifier
--  args <- between (symbol "(") (symbol ")") (exprParser `sepBy` symbol ",")
--  return $ Call func args

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
  symbol "{"
  stmts <- many statementParser
  symbol "}"
  return stmts

callStmt :: Parser Statement
callStmt = do
  _ <- symbol "call"
  name <- identifier
  return $ Call name



