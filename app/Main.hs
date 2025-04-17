module Main where

import RVRS.AST
import RVRS.Codegen (generateAiken)

main :: IO ()
main = do
  let testFlow = Flow
        { flowName = "grant_access"
        , flowArgs = ["user: Identity"]
        , flowBody =
            [ Source "trust" (Call "check_trust" [Var "user"])
            , Branch (Equals (Var "trust") (BoolLit True))
                [ Delta "state" (StrLit "allowed")
                , Mouth (StrLit "Access granted.")
                ]
                [ Mouth (StrLit "Access denied.")
                , Delta "state" (StrLit "denied")
                ]
            , Echo (Var "state")
            ]
        }

  putStrLn $ generateAiken testFlow
