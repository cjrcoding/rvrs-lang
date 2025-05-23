-- src/RVRS/Eval.hs
module RVRS.Eval (
  evalIRExpr,
  evalIRStmt,
  evalStmtsWithEnv,
  evalIRFlow,
  runEvalIR,
  EvalError(..)
) where

import RVRS.Eval.EvalExpr (evalIRExpr)
import RVRS.Eval.EvalStmt (evalIRStmt, evalStmtsWithEnv)
import RVRS.Eval.EvalFlow (evalIRFlow, runEvalIR, EvalError(..))
