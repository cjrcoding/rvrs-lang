-- src/RVRS/Eval.hs
module RVRS.Eval (
  evalExpr,
  evalIRStmt,
  evalBody,
  evalIRFlow,
  runEvalIR,
  EvalError(..)
) where

import RVRS.Eval.EvalExpr (evalExpr, evalBody)
import RVRS.Eval.EvalStmt (evalIRStmt)
import RVRS.Eval.EvalFlow (evalIRFlow, runEvalIR, EvalError(..))
