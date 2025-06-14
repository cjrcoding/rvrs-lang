module RVRS.Typecheck.Check where

import Ya (Recursive (..), is, unwrap, ha__, hu, la, li)
import RVRS.AST
import RVRS.Typecheck.Types
import qualified Data.Map as Map

typeOfExpr :: TypeEnv -> Recursive Expression -> Either TypeError RVRS_Type
typeOfExpr env expr = case unwrap expr of
  Literal x -> Right (is @String `hu` TStr `la` is @Double `hu` TNum `la` is @Bool `hu` TBool `li` x)
  Variable x ->
    case Map.lookup x env of
      Just t  -> Right t
      Nothing -> Left $ UnknownVariable x

  Operator (Binary (Add a b)) -> checkBinary env TNum TNum a b
  Operator (Binary (Sub a b)) -> checkBinary env TNum TNum a b
  Operator (Binary (Mul a b)) -> checkBinary env TNum TNum a b
  Operator (Binary (Div a b)) -> checkBinary env TNum TNum a b

  Operator (Binary (Equals a b)) -> do
    t1 <- typeOfExpr env a
    t2 <- typeOfExpr env b
    case t1 == t2 of
      True  -> Right TBool
      False -> Left $ TypeMismatch t1 t2

  Operator (Binary (And a b)) -> checkBinary env TBool TBool a b
  Operator (Binary (Or a b))  -> checkBinary env TBool TBool a b

  Operator (Unary (Not e)) -> do
    t <- typeOfExpr env e
    case t of
      TBool -> Right TBool
      _     -> Left $ TypeMismatch t TBool

  Operator (Unary (Neg e)) -> do
    t <- typeOfExpr env e
    case t of
      TNum -> Right TNum
      _    -> Left $ TypeMismatch t TNum

  Operator (Binary (Greater a b)) -> checkBinary env TNum TBool a b
  Operator (Binary (Less a b)) -> checkBinary env TNum TBool a b

  other -> Left $ UnsupportedOp (show other)


checkBinary :: TypeEnv -> RVRS_Type -> RVRS_Type -> Recursive Expression -> Recursive Expression -> Either TypeError RVRS_Type
checkBinary env expected retType a b = do
  t1 <- typeOfExpr env a
  t2 <- typeOfExpr env b
  case (t1, t2) of
    (t1', t2') | t1' == expected && t2' == expected -> Right retType
               | otherwise -> Left $ TypeMismatch t1' t2'