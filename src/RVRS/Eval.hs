-- src/RVRS/Eval.hs
module RVRS.Eval (
  evalIRExpr,
  evalIRStmt,
  evalBody,
  evalIRFlow,
  runEvalIR,
  EvalError(..)
) where

import RVRS.Eval.EvalExpr (evalIRExpr, evalBody)
import RVRS.Eval.EvalStmt (evalIRStmt)
import RVRS.Eval.EvalFlow (evalIRFlow, runEvalIR, EvalError(..))
