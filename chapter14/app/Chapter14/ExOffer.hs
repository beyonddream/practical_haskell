module Chapter14.ExOffer where

import Chapter14.Offer
import qualified Text.Blaze.Html5 as H

describeOffer :: (Eq a, Show a) => Offer a -> H.Html
describeOffer o =
  html_Syn_HtmlRoot $
  wrap_HtmlRoot (sem_HtmlRoot $ HtmlRoot_HtmlRoot o) Inh_HtmlRoot
