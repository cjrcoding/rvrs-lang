-- src/RVRS/Eval.hs
module RVRS.Eval (
  evalIRExpr,
  evalIRStmt,
  evalStmtsWithEnv,
  evalIRFlow,
  runEvalIR,
  EvalError(..)
) where

import RVRS.Eval.Expr (evalIRExpr)
import RVRS.Eval.Stmt (evalIRStmt, evalStmtsWithEnv)
import RVRS.Eval.Flow (evalIRFlow, runEvalIR, EvalError(..))
