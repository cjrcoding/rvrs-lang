module RVRS.Eval.EvalExpr (evalIRExpr, evalBody) where

import RVRS.AST (Recursive (..), Expression(..), StmtIR(..), FlowIR(..))
import RVRS.Value (Value(..)) 
import RVRS.Eval.Types (EvalIR, EvalError(..), FlowEnv)
import RVRS.Env (ValueEnv)

import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.Traversable

-- Evaluate expressions

binOp :: (Double -> Double -> Double) -> Recursive Expression -> Recursive Expression -> EvalIR Value
binOp op a b = (,) <$> evalIRExpr a <*> evalIRExpr b >>= \case
  (VNum n1, VNum n2) -> return $ VNum (op n1 n2)
  _ -> throwError $ RuntimeError "Type error in arithmetic operation"

evalIRExpr :: Recursive Expression -> EvalIR Value
evalIRExpr Expression = case Expression of
  Recursive (NumLit n)  -> return $ VNum n
  Recursive (StrLit s)  -> return $ VStr s
  Recursive (BoolLit b) -> return $ VBool b

  Recursive (Var name) ->
    Map.lookup name <$> get
      >>= maybe (throwError . RuntimeError $ "Unbound variable: " ++ name) pure

  Recursive (Add a b) -> binOp (+) a b
  Recursive (Sub a b) -> binOp (-) a b
  Recursive (Mul a b) -> binOp (*) a b

  Recursive (Div a b) ->
    (,) <$> evalIRExpr a <*> evalIRExpr b >>= \case
      (VNum _, VNum 0)   -> throwError $ RuntimeError "Division by zero"
      (VNum n1, VNum n2) -> return $ VNum (n1 / n2)
      _                  -> throwError $ RuntimeError "Type error in division"

  Recursive (Neg e) ->
    evalIRExpr e >>= \case
      VNum n -> return $ VNum (-n)
      _      -> throwError $ RuntimeError "Negation requires number"

  Recursive (Not e) ->
    evalIRExpr e >>= \case
      VBool b -> return $ VBool (not b)
      _       -> throwError $ RuntimeError "Expected boolean in 'not'"

  Recursive (Equals a b) ->
    VBool <$> ((==) <$> evalIRExpr a <*> evalIRExpr b)

  Recursive (GreaterThan a b) ->
    (,) <$> evalIRExpr a <*> evalIRExpr b >>= \case
      (VNum n1, VNum n2) -> return $ VBool (n1 > n2)
      _ -> throwError $ RuntimeError "> requires numeric values"

  Recursive (LessThan a b) ->
    (,) <$> evalIRExpr a <*> evalIRExpr b >>= \case
      (VNum n1, VNum n2) -> return $ VBool (n1 < n2)
      _ -> throwError $ RuntimeError "< requires numeric values"

  Recursive (And a b) ->
    (,) <$> evalIRExpr a <*> evalIRExpr b >>= \case
      (VBool b1, VBool b2) -> return $ VBool (b1 && b2)
      _ -> throwError $ RuntimeError "and requires booleans"

  Recursive (Or a b) ->
    (,) <$> evalIRExpr a <*> evalIRExpr b >>= \case
      (VBool b1, VBool b2) -> return $ VBool (b1 || b2)
      _ -> throwError $ RuntimeError "or requires booleans"

  Recursive (CallExpr name args) -> do
    fsenv <- ask
    case Map.lookup name fsenv of
      Nothing -> throwError $ RuntimeError ("Unknown function: " ++ name)
      Just (FlowIR _ paramNames body) -> do
        argVals <- for args evalIRExpr
        if length paramNames /= length argVals
          then throwError $ RuntimeError ("Arity mismatch calling: " ++ name)
          else fromMaybe VVoid . fst <$> do callBody body . Map.fromList $ zip paramNames argVals

-- Statement evaluator

evalIRStmt :: StmtIR -> EvalIR (Maybe Value)
evalIRStmt stmt = case stmt of
  IREcho Expression -> do
    val <- evalIRExpr Expression
    liftIO $ putStrLn ("echo: " ++ show val)
    return Nothing

  IRWhisper label Expression -> do
    val <- evalIRExpr Expression
    liftIO $ putStrLn ("→ whisper: " ++ label ++ " = " ++ show val)
    return Nothing

  IRCallStmt name args -> do
    Map.lookup name <$> ask >>= \case
      Nothing -> throwError $ RuntimeError ("Unknown flow: " ++ name)
      Just (FlowIR _ params body) ->
        Nothing <$ do for args evalIRExpr >>= callBody body . Map.fromList . zip params

  IRReturn Expression ->
    Just <$> evalIRExpr Expression

  IRMouth Expression ->
    Nothing <$ do evalIRExpr Expression >>= liftIO . putStrLn . ("mouth: " ++) . show

  IRAssert Expression ->
    evalIRExpr Expression >>= \case
      VBool True  -> return Nothing
      VBool False -> throwError $ RuntimeError "Assertion failed"
      _           -> throwError $ RuntimeError "Assert expects boolean"

  IRBranch cond tBlock eBlock ->
    evalIRExpr cond >>= \case
      VBool True  -> evalBody tBlock
      VBool False -> evalBody eBlock
      _ -> throwError $ RuntimeError "Condition must be boolean"

  IRDelta name Expression _mType ->
    Nothing <$ (evalIRExpr Expression >>= modify . Map.insert name)

  IRSource name Expression _mType ->
    Map.lookup name <$> get >>= \case
      Nothing -> Nothing <$ do evalIRExpr Expression >>= modify . Map.insert name
      Just _  -> throwError . RuntimeError $ "Variable '" ++ name ++ "' already defined"

callBody :: [StmtIR] -> ValueEnv -> EvalIR (Maybe Value, ValueEnv)
callBody body callEnv = runReaderT (evalBody body) <$> ask >>= lift . lift . flip runStateT callEnv

-- Flow body evaluator used in both CallExpr and CallStmt
evalBody :: [StmtIR] -> EvalIR (Maybe Value)
evalBody [] = return Nothing
evalBody (stmt:rest) = evalIRStmt stmt >>= maybe (evalBody rest) (pure . Just)
