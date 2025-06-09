module RVRS.Eval.EvalExpr (evalExpr, evalBody) where

import RVRS.AST (Recursive (..), Expression(..), Statement(..), FlowIR(..))
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
binOp op a b = (,) <$> evalExpr a <*> evalExpr b >>= \case
  (VNum n1, VNum n2) -> return $ VNum (op n1 n2)
  _ -> throwError $ RuntimeError "Type error in arithmetic operation"

evalExpr :: Recursive Expression -> EvalIR Value
evalExpr expr = case expr of
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
    (,) <$> evalExpr a <*> evalExpr b >>= \case
      (VNum _, VNum 0)   -> throwError $ RuntimeError "Division by zero"
      (VNum n1, VNum n2) -> return $ VNum (n1 / n2)
      _                  -> throwError $ RuntimeError "Type error in division"

  Recursive (Neg e) ->
    evalExpr e >>= \case
      VNum n -> return $ VNum (-n)
      _      -> throwError $ RuntimeError "Negation requires number"

  Recursive (Not e) ->
    evalExpr e >>= \case
      VBool b -> return $ VBool (not b)
      _       -> throwError $ RuntimeError "Expected boolean in 'not'"

  Recursive (Equals a b) ->
    VBool <$> ((==) <$> evalExpr a <*> evalExpr b)

  Recursive (GreaterThan a b) ->
    (,) <$> evalExpr a <*> evalExpr b >>= \case
      (VNum n1, VNum n2) -> return $ VBool (n1 > n2)
      _ -> throwError $ RuntimeError "> requires numeric values"

  Recursive (LessThan a b) ->
    (,) <$> evalExpr a <*> evalExpr b >>= \case
      (VNum n1, VNum n2) -> return $ VBool (n1 < n2)
      _ -> throwError $ RuntimeError "< requires numeric values"

  Recursive (And a b) ->
    (,) <$> evalExpr a <*> evalExpr b >>= \case
      (VBool b1, VBool b2) -> return $ VBool (b1 && b2)
      _ -> throwError $ RuntimeError "and requires booleans"

  Recursive (Or a b) ->
    (,) <$> evalExpr a <*> evalExpr b >>= \case
      (VBool b1, VBool b2) -> return $ VBool (b1 || b2)
      _ -> throwError $ RuntimeError "or requires booleans"

  Recursive (CallExpr name args) -> do
    fsenv <- ask
    case Map.lookup name fsenv of
      Nothing -> throwError $ RuntimeError ("Unknown function: " ++ name)
      Just (FlowIR _ paramNames body) -> do
        argVals <- for args evalExpr
        if length paramNames /= length argVals
          then throwError $ RuntimeError ("Arity mismatch calling: " ++ name)
          else fromMaybe VVoid . fst <$> do callBody body . Map.fromList $ zip paramNames argVals

-- Statement evaluator

evalStmt :: Recursive Statement -> EvalIR (Maybe Value)
evalStmt stmt = case stmt of
  Recursive (Echo expr) -> do
    val <- evalExpr expr
    liftIO $ putStrLn ("echo: " ++ show val)
    return Nothing

  Recursive (Whisper expr) -> do
    val <- evalExpr expr
    liftIO $ putStrLn ("→ whisper: " ++ " = " ++ show val)
    return Nothing

  Recursive (Call name args) -> do
    Map.lookup name <$> ask >>= \case
      Nothing -> throwError $ RuntimeError ("Unknown flow: " ++ name)
      Just (FlowIR _ params body) ->
        Nothing <$ do for args evalExpr >>= callBody body . Map.fromList . zip params

  Recursive (Return expr) ->
    Just <$> evalExpr expr

  Recursive (Mouth expr) ->
    Nothing <$ do evalExpr expr >>= liftIO . putStrLn . ("mouth: " ++) . show

  Recursive (Assert expr) ->
    evalExpr expr >>= \case
      VBool True  -> return Nothing
      VBool False -> throwError $ RuntimeError "Assertion failed"
      _           -> throwError $ RuntimeError "Assert expects boolean"

  Recursive (Branch cond tBlock eBlock) ->
    evalExpr cond >>= \case
      VBool True  -> evalBody tBlock
      VBool False -> evalBody eBlock
      _ -> throwError $ RuntimeError "Condition must be boolean"

  Recursive (Delta name _mType expr) ->
    Nothing <$ (evalExpr expr >>= modify . Map.insert name)

  Recursive (Source name _mType expr) ->
    Map.lookup name <$> get >>= \case
      Nothing -> Nothing <$ do evalExpr expr >>= modify . Map.insert name
      Just _  -> throwError . RuntimeError $ "Variable '" ++ name ++ "' already defined"

callBody :: [Recursive Statement] -> ValueEnv -> EvalIR (Maybe Value, ValueEnv)
callBody body callEnv = runReaderT (evalBody body) <$> ask >>= lift . lift . flip runStateT callEnv

-- Flow body evaluator used in both CallExpr and CallStmt
evalBody :: [Recursive Statement] -> EvalIR (Maybe Value)
evalBody [] = return Nothing
evalBody (stmt:rest) = evalStmt stmt >>= maybe (evalBody rest) (pure . Just)
