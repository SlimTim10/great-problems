module Problem.Editor
  ( widget
  ) where

import qualified Reflex.Dom.Core as R

import Global

widget
  :: ( R.DomBuilder t m
     )
  => R.Event t Text
  -> m (R.Dynamic t Text)
widget forcedValue = R.elClass "div" "h-full" $ do
  t <- R.textAreaElement $ R.def
    & R.setValue .~ forcedValue
    & R.initialAttributes .~ ("class" =: "h-full w-full border")
  return $ R.value t
