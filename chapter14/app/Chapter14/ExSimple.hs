module Chapter14.ExSimple where

import Chapter14.Simple

executeExpr ::
     Ord a => Expr a -> [(a, Float)] -> (Maybe Int, Maybe Float, Maybe Bool)
executeExpr e products =
  let syn = wrap_Expr (sem_Expr e) (Inh_Expr products) -- returns Syn_Expr
   in (intValue_Syn_Expr syn, fltValue_Syn_Expr syn, boolValue_Syn_Expr syn)
