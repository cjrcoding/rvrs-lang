module RVRS.Parser.Type (RVRSType(..), typeParser) where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- The core type tags used for annotations
data RVRSType
  = TypeNum
  | TypeStr
  | TypeBool
  | TypeAny   -- Optional catch-all
  deriving (Eq, Show)

-- Parser for these types (used in annotations)
typeParser :: Parsec Void String RVRSType
typeParser = choice
  [ symbol "Num"  >> return TypeNum
  , symbol "Str"  >> return TypeStr
  , symbol "Bool" >> return TypeBool
  ]

-- Local symbol parser
sc :: Parsec Void String ()
sc = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

symbol :: String -> Parsec Void String String
symbol = L.symbol sc
