-- src/RVRS/Parser/Type.hs

module RVRS.Parser.Type
  ( RVRSType(..)
  , typeParser
  , parseType
  ) where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- | RVRS-supported type annotations
data RVRSType
  = TypeNum
  | TypeStr
  | TypeBool
  | TypeAny  -- Optional fallback
  deriving (Eq, Show)

-- | Parser for type keywords like 'Num', 'Str', etc.
typeParser :: Parsec Void String RVRSType
typeParser = choice
  [ symbol "Num"  >> return TypeNum
  , symbol "Str"  >> return TypeStr
  , symbol "Bool" >> return TypeBool
  ]

-- | Alias for compatibility across modules
parseType :: Parsec Void String RVRSType
parseType = typeParser

-- | Whitespace and comment skipping
sc :: Parsec Void String ()
sc = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

-- | Lexeme-aware symbol parser
symbol :: String -> Parsec Void String String
symbol = L.symbol sc
