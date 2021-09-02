module Problem.Summary
  ( widget
  ) where

import qualified Reflex.Dom.Core as R

import qualified Widget.Input as Input
import Global

widget
  :: ( R.DomBuilder t m
     )
  => m (R.Dynamic t Text)
widget = R.elClass "div" "" $ do
  R.elClass "p" "font-medium mb-2" $ R.text "Summary"
  Input.textAreaClass "border border-brand-light-gray w-full px-1"
