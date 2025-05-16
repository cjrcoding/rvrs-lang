module RVRS.Value (Value(..)) where

-- Values produced during RVRS evaluation
data Value
  = VNum Double
  | VStr String
  | VBool Bool
  | VError String
  deriving (Eq, Show)
