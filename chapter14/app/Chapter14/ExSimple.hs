module Chapter14.ExSimple where

import Chapter14.Simple

executeExpr :: Expr -> String -> Int
executeExpr e s =
  let syn = wrap_Expr (sem_Expr e) (Inh_Expr s)  -- returns Syn_Expr
   in result_Syn_Expr syn
