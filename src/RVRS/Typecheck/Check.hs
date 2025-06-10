module RVRS.Typecheck.Check where

import RVRS.AST
import RVRS.Typecheck.Types
import qualified Data.Map as Map

typeofExpr :: TypeEnv -> Recursive Expression -> Either TypeError RVRS_Type
typeofExpr env (Recursive expr) = case expr of
  NumLit _ -> Right TNum
  StrLit _ -> Right TStr
  BoolLit _ -> Right TBool
  Var x ->
    case Map.lookup x env of
      Just t  -> Right t
      Nothing -> Left $ UnknownVariable x

  Add a b -> checkBinary env TNum TNum a b
  Sub a b -> checkBinary env TNum TNum a b
  Mul a b -> checkBinary env TNum TNum a b
  Div a b -> checkBinary env TNum TNum a b

  Equals a b -> do
    t1 <- typeofExpr env a
    t2 <- typeofExpr env b
    case t1 == t2 of
      True  -> Right TBool
      False -> Left $ TypeMismatch t1 t2

  And a b -> checkBinary env TBool TBool a b
  Or a b  -> checkBinary env TBool TBool a b

  Not e -> do
    t <- typeofExpr env e
    case t of
      TBool -> Right TBool
      _     -> Left $ TypeMismatch t TBool

  Neg e -> do
    t <- typeofExpr env e
    case t of
      TNum -> Right TNum
      _    -> Left $ TypeMismatch t TNum

  GreaterThan a b -> checkBinary env TNum TBool a b
  LessThan a b    -> checkBinary env TNum TBool a b

  _ -> Left $ UnsupportedOp (show expr)


checkBinary :: TypeEnv -> RVRS_Type -> RVRS_Type -> Recursive Expression -> Recursive Expression -> Either TypeError RVRS_Type
checkBinary env expected retType a b = do
  t1 <- typeofExpr env a
  t2 <- typeofExpr env b
  case (t1, t2) of
    (t1', t2') | t1' == expected && t2' == expected -> Right retType
               | otherwise -> Left $ TypeMismatch t1' t2'
