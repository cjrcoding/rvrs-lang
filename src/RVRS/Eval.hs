-- src/RVRS/Eval.hs
module RVRS.Eval (
  evalExpr,
  evalStmt,
  evalBody,
  evalIRFlow,
  runEvalIR,
  EvalError(..)
) where

import RVRS.Eval.EvalExpr (evalExpr, evalBody)
import RVRS.Eval.EvalStmt (evalStmt)
import RVRS.Eval.EvalFlow (evalIRFlow, runEvalIR, EvalError(..))
