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
widget forcedValue = R.elAttr "div" ("style" =: "border: 1px solid black;") $ do
  R.el "h2" $ R.text "Editor"
  t <- R.textAreaElement $ R.def & R.setValue .~ forcedValue
  return $ R.value t
