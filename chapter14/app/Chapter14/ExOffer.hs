module Chapter14.ExOffer where

import Chapter14.Offer

executeOffer :: Ord a => Offer a -> ([a], Int)
executeOffer e =
  let syn = wrap_Offer (sem_Offer e) Inh_Offer -- returns Syn_Offer
   in (presents_Syn_Offer syn, maxDuration_Syn_Offer syn)
