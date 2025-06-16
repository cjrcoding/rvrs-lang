module RVRS.Typecheck.Check where

import Ya (Recursive (..), pattern Unit, is, unwrap, ha__, hu, la, li)
import RVRS.AST
import RVRS.Value
import RVRS.Typecheck.Types
import qualified Data.Map as Map

typeOfExpr :: TypeEnv -> Recursive Expression -> Either TypeError Typed
typeOfExpr env expr = case unwrap expr of
  Lit x -> Right (valueToType x)
  Var x ->
    case Map.lookup x env of
      Just t  -> Right t
      Nothing -> Left $ UnknownVariable x

  Add a b -> checkBinary env (Double Unit) (Double Unit) a b
  Sub a b -> checkBinary env (Double Unit) (Double Unit) a b
  Mul a b -> checkBinary env (Double Unit) (Double Unit) a b
  Div a b -> checkBinary env (Double Unit) (Double Unit) a b

  Equals a b -> do
    t1 <- typeOfExpr env a
    t2 <- typeOfExpr env b
    case t1 == t2 of
      True  -> Right (Bool Unit)
      False -> Left $ TypeMismatch t1 t2

  And a b -> checkBinary env (Bool Unit) (Bool Unit) a b
  Or a b  -> checkBinary env (Bool Unit) (Bool Unit) a b

  Not e -> do
    t <- typeOfExpr env e
    case t of
      Bool Unit -> Right (Bool Unit)
      _     -> Left $ TypeMismatch t (Bool Unit)

  Neg e -> do
    t <- typeOfExpr env e
    case t of
      Double Unit -> Right (Double Unit)
      _    -> Left $ TypeMismatch t (Double Unit)

  GreaterThan a b -> checkBinary env (Double Unit) (Bool Unit) a b
  LessThan a b    -> checkBinary env (Double Unit) (Bool Unit) a b

  other -> Left $ UnsupportedOp (show other)


checkBinary :: TypeEnv -> Typed -> Typed -> Recursive Expression -> Recursive Expression -> Either TypeError Typed
checkBinary env expected retType a b = do
  t1 <- typeOfExpr env a
  t2 <- typeOfExpr env b
  case (t1, t2) of
    (t1', t2') | t1' == expected && t2' == expected -> Right retType
               | otherwise -> Left $ TypeMismatch t1' t2'
