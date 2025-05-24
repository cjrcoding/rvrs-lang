-- src/RVRS/Eval.hs
module RVRS.Eval (
  evalIRExpr,
  evalIRStmt,
  evalIRFlow,
  runEvalIR,
  EvalError(..)
) where


import RVRS.Eval.EvalExpr (evalIRExpr)
import RVRS.Eval.EvalStmt (evalIRStmt)
import RVRS.Eval.SharedCall (evalStmtsWithEnv)
import RVRS.Eval.EvalFlow (evalIRFlow, runEvalIR, EvalError(..))
