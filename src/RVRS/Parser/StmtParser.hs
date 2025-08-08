module RVRS.Parser.StmtParser (statementParser, blockParser) where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad (void)
import Control.Monad.Combinators.Expr
import GHC.IsList (fromList)
import Data.Void
import Data.Char (isAlphaNum)

import Ya (Recursive (..), ho'ho, ho'ho'ho)

import RVRS.AST
import RVRS.Parser.ExprParser (exprParser, identifier)
-- import RVRS.Parser.Type (typeParser)

type Parser = Parsec Void String

statementParser :: Parser (Recursive Statement)
statementParser = choice (
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
  ] :: [Parser (Recursive Statement)])

-- Individual statement parsers

whisperParser :: Parser (Recursive Statement)
whisperParser = symbol "whisper" *> do Recursive <$> Whisper <$> exprParser

assertParser :: Parser (Recursive Statement)
assertParser = symbol "assert" *> do Recursive <$> Assert <$> exprParser

mouthParser :: Parser (Recursive Statement)
mouthParser = symbol "mouth" *> do Recursive <$> Mouth <$> exprParser

echoParser :: Parser (Recursive Statement)
echoParser = symbol "echo" *> do Recursive <$> Echo <$> exprParser

speaksParser :: Parser (Recursive Statement)
speaksParser = symbol "speaks" *> do Recursive <$> Echo <$> exprParser

sourceParser :: Parser (Recursive Statement)
sourceParser = Source `ho'ho` Recursive
  <$> do symbol "source" *> identifier
  -- TODO: there is actually a better way to describe it
  -- <*> do try (symbol ":" *> (Just <$> typeParser)) <|> pure Nothing
  <*> do symbol "=" *> exprParser

-- Delta parser supporting both typed and untyped declarations
deltaParser :: Parser (Recursive Statement)
deltaParser = Delta `ho'ho` Recursive
  <$> do symbol "delta" *> identifier
  -- <*> do try (symbol ":" *> do Just <$> typeParser) <|> pure Nothing
  <*> do symbol "=" *> exprParser

pillarParser :: Parser (Recursive Statement)
pillarParser = Pillar `ho'ho` Recursive
  <$> do symbol ("pillar" :: String) *> identifier
  <*> do symbol ("=" :: String) *> exprParser

returnParser :: Parser (Recursive Statement)
returnParser = Recursive <$> Return <$> do symbol "return" *> exprParser

branchParser :: Parser (Recursive Statement)
branchParser = Branch `ho'ho'ho` Recursive
  <$> do symbol "branch" *> exprParser
  <*> do fromList <$> do sc *> blockParser
  <*> do fromList <$> do maybe [] id <$> do optional . try $ sc *> symbol "else" *> sc *> blockParser

blockParser :: Parser [Recursive Statement]
blockParser = between (symbol "{") (symbol "}") (many (sc *> statementParser <* sc))

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "--"
    blockCmnt = L.skipBlockComment "{-" "-}"
