module RVRS.Parser.StmtParser (statementParser, blockParser) where

import RVRS.AST
import RVRS.Parser.ExprParser (exprParser)
import RVRS.Parser.Type (typeParser)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad (void)
import Control.Monad.Combinators.Expression
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
whisperParser = symbol "whisper" *> do Whisper <$> exprParser

assertParser :: Parser Statement
assertParser = symbol "assert" *> do Assert <$> exprParser

mouthParser :: Parser Statement
mouthParser = symbol "mouth" *> do Mouth <$> exprParser

echoParser :: Parser Statement
echoParser = symbol "echo" *> do Echo <$> exprParser

speaksParser :: Parser Statement
speaksParser = symbol "speaks" *> do Echo <$> exprParser

sourceParser :: Parser Statement
sourceParser = Source
  <$> do symbol "source" *> identifier
  -- TODO: there is actually a better way to describe it
  <*> do try (symbol ":" *> (Just <$> typeParser)) <|> pure Nothing
  <*> do symbol "=" *> exprParser

bareCallStmt :: Parser Statement
bareCallStmt = Call
  <$> identifier
  <*> between (symbol "(") (symbol ")") (exprParser `sepBy` symbol ",")

-- Delta parser supporting both typed and untyped declarations
deltaParser :: Parser Statement
deltaParser = Delta
  <$> do symbol "delta" *> identifier
  <*> do try (symbol ":" *> do Just <$> typeParser) <|> pure Nothing
  <*> do symbol "=" *> exprParser

pillarParser :: Parser Statement
pillarParser = Pillar
  <$> do symbol "pillar" *> identifier
  <*> do symbol "=" *> exprParser

returnParser :: Parser Statement
returnParser = Return <$> do symbol "return" *> exprParser

branchParser :: Parser Statement
branchParser = Branch
  <$> do symbol "branch" *> exprParser
  <*> do sc *> blockParser
  <*> do maybe [] id <$> do optional . try $ sc *> symbol "else" *> sc *> blockParser

blockParser :: Parser [Statement]
blockParser = between (symbol "{") (symbol "}") (many (sc *> statementParser <* sc))

callStmt :: Parser Statement
callStmt = Call
  <$> do symbol "call" *> identifier
  <*> do option [] . parens $ exprParser `sepBy` symbol ","

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
