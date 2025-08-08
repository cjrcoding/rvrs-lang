module RVRS.Parser.Type () where

import Data.Void
import Data.Functor (($>))
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Ya (pattern Unit)

import RVRS.Syntax (type Typed, pattern Double, pattern String, pattern Bool)

-- Parser for these types (used in annotations)
-- typeParser :: Parsec Void String Typed
-- typeParser = choice
  -- [ symbol "Num"  $> Double Unit
  -- , symbol "Str"  $> String Unit
  -- , symbol ("Bool" :: String) $> Bool Unit
  -- ]

-- Local symbol parser
-- sc :: Parsec Void String ()
-- sc = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

-- symbol :: String -> Parsec Void String String
-- symbol = L.symbol sc
