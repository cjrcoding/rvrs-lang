module RVRS.Types
  ( RVRSValueType(..)
  , RVRSType(..)             
  , TypeError(..)
  , TypedVal(..)
  , TypeEnv
  , FlowSig(..)              
  , FlowSigEnv
  ) where

import qualified Data.Map as Map
import RVRS.Value (Value)
import RVRS.Parser.Type (RVRSType(..)) 

-- Supported types in RVRS (runtime values)
data RVRSValueType = VNum | VStr | VBool
  deriving (Eq, Show)

-- Type error representation
data TypeError
  = TypeMismatch String
  | UnknownVariable String
  deriving (Eq, Show)

-- Type + Value pair for type checker
data TypedVal = TypedVal RVRSValueType Value
  deriving (Show, Eq)

-- Variable name → typed value
type TypeEnv = Map.Map String TypedVal

-- Flow signature for type checking
data FlowSig = FlowSig
  { argTypes   :: [RVRSValueType]
  , returnType :: RVRSValueType
  } deriving (Show, Eq)

-- Flow name → signature mapping
type FlowSigEnv = Map.Map String FlowSig
