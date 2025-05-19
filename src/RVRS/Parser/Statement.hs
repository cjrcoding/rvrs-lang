module RVRS.Parser.Statement (statementParser, blockParser) where

import RVRS.AST
import RVRS.Parser.Expression (exprParser)
import RVRS.Parser.Type (typeParser)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Void
import Data.Char (isAlphaNum)

type Parser = Parsec Void String

statementParser = choice
  [ try pillarParser
  , try mouthParser
  , try whisperParser     
  , try assertParser
  , try speaksParser
  , try echoParser
  , try sourceParser
  , try deltaParser
  , try branchParser
  , try returnParser
  , try callStmt           
  , try bareCallStmt
  ]


-- Individual statement parsers

whisperParser :: Parser Statement
whisperParser = do
  _ <- symbol "whisper"
  expr <- exprParser
  return $ Whisper expr

assertParser :: Parser Statement
assertParser = do
  _ <- symbol "assert"
  expr <- exprParser
  return $ Assert expr

mouthParser :: Parser Statement
mouthParser = do
  _ <- symbol "mouth"
  expr <- exprParser
  return $ Mouth expr

echoParser :: Parser Statement
echoParser = do
  _ <- symbol "echo"
  expr <- exprParser
  return $ Echo expr

speaksParser :: Parser Statement
speaksParser = do
  symbol "speaks"
  expr <- exprParser
  return $ Echo expr


sourceParser :: Parser Statement
sourceParser = do
  _ <- symbol "source"
  var <- identifier
  _ <- symbol "="
  expr <- exprParser
  return $ Source var expr


bareCallStmt :: Parser Statement
bareCallStmt = do
  name <- identifier
  args <- between (symbol "(") (symbol ")") (exprParser `sepBy` symbol ",")
  return $ Call name args


-- Delta parser supporting both typed and untyped declarations

deltaParser :: Parser Statement
deltaParser = try typedDelta <|> try equalsDelta

typedDelta :: Parser Statement
typedDelta = do
  _ <- symbol "delta"
  var <- identifier
  _ <- symbol ":"
  typ <- typeParser
  _ <- symbol "="
  expr <- exprParser
  return $ Delta var (Just typ) expr

equalsDelta :: Parser Statement
equalsDelta = do
  _ <- symbol "delta"
  var <- identifier
  _ <- symbol "="
  expr <- exprParser
  return $ Delta var Nothing expr

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

branchParser :: Parser Statement
branchParser = do
  _ <- symbol "branch"
  cond <- exprParser
  thenBlock <- sc *> blockParser
  elseBlock <- optional (try (sc *> symbol "else" *> sc *> blockParser))
  return $ Branch cond thenBlock (maybe [] id elseBlock)

blockParser :: Parser [Statement]
blockParser = between (symbol "{") (symbol "}") (many (sc *> statementParser <* sc))

callStmt :: Parser Statement
callStmt = do
  _ <- symbol "call"
  name <- identifier
  args <- option [] (parens (exprParser `sepBy` symbol ","))
  return $ Call name args

-- Shared utilities

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

identifier :: Parser String
identifier = lexeme $ (:) <$> letterChar <*> many (alphaNumChar <|> char '_')

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "--"
    blockCmnt = L.skipBlockComment "{-" "-}"
