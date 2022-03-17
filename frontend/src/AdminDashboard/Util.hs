module AdminDashboard.Util where

import Common.Lib.Prelude

import qualified Reflex.Dom.Core as R

title
  :: ( R.DomBuilder t m
     )
  => Text
  -> m ()
title x = R.elClass "p" "text-center text-brand-lg font-normal" $ R.text x
