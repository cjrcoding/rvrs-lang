-- src/RVRS/AST.hs

module RVRS.AST where

import RVRS.Parser.Type (RVRSType(..))


-- | Represents a named flow of ritual logic
data Flow = Flow
  { flowName :: String           -- ^ The flow's name (e.g. "main", "add")
  , flowArgs :: [Argument]       -- ^ Named ritual arguments, optionally typed
  , flowBody :: [Statement]      -- ^ The ritual body (statements)
  } deriving (Show, Eq)

-- | A named argument to a flow, e.g., `x: Number`
data Argument = Argument
  { argName :: String            -- ^ The argument name
  , argType :: String            -- ^ Placeholder for the type (optional for now)
  } deriving (Show, Eq)


-- | Statements inside a flow block
data Statement
  = Source String (Maybe RVRSType) Expr                 -- source x = ...
  | Delta String (Maybe RVRSType) Expr
  | Branch Expr [Statement] [Statement]-- branch cond { ... } else { ... }
  | Mouth Expr                          -- mouth "..."
  | Whisper Expr
  | Echo Expr                           -- echo x
  | Pillar String Expr  -- pillar NAME = ...
  | Return Expr
  | Call String [Expr]
  | Assert Expr

  deriving (Show, Eq)


data Expr
  = Var String                          -- x
  | StrLit String                       -- "hello"
  | BoolLit Bool                        -- true, false
  | Equals Expr Expr                    -- a == b
  | GreaterThan Expr Expr              -- a > b
  | LessThan Expr Expr                 -- a < b
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | NumLit Double
  | Not Expr     
  | And Expr Expr   
  | Or Expr Expr 
  | CallExpr String [Expr]   
  | Neg Expr 
  
  
  
  deriving (Show, Eq)


  
