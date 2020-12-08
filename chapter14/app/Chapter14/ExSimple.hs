module Chapter14.ExSimple where

import Chapter14.Simple

executeExpr ::
     Ord a => Root a -> [(a, Float)] -> (Maybe Int, Maybe Float, Maybe Bool)
executeExpr e products =
  let syn = wrap_Root (sem_Root e) (Inh_Root products) -- returns Syn_Root
   in (intValue_Syn_Root syn, fltValue_Syn_Root syn, boolValue_Syn_Root syn)
