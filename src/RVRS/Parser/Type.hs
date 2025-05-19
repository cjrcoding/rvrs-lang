-- src/RVRS/Parser/Type.hs
module RVRS.Parser.Type (RVRSType(..), typeParser) where

-- Internal modules
import RVRS.AST

-- External libraries
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


typeParser :: Parsec Void String RVRSType
typeParser = choice
  [ symbol "Num"  >> return TypeNum
  , symbol "Str"  >> return TypeStr
  , symbol "Bool" >> return TypeBool
  ]

-- Local copy of symbol for now
sc :: Parsec Void String ()
sc = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

symbol :: String -> Parsec Void String String
symbol = L.symbol sc
