module Header
  ( widget
  ) where

import qualified Reflex.Dom.Core as R
import qualified MyReflex.Dom.Widget.Basic as R'

import Global

widget
  :: forall t m.
     ( R.DomBuilder t m
     )
  => m ()
widget = R.elClass
  "header"
  "h-14 py-2 px-3 flex items-center justify-between border-b" $ do
    R.elClass "p" "font-medium text-xl" $ R.text "Great Problems"
    secondaryButton "Explore"
    R'.elAttrClass
      "input"
      ("type" =: "text" <> "placeholder" =: "Search...")
      "border rounded h-8 w-1/2 px-1"
      $ R.blank
    R.el "div" $ do
      primaryButton "Create an account"
      secondaryButton "Sign in"

primaryButton
  :: forall t m.
     ( R.DomBuilder t m
     )
  => Text
  -> m ()
primaryButton t = R'.elAttrClass
  "button"
  ("type" =: "button")
  "bg-brand-primary rounded text-white font-medium px-3 h-10 mr-3"
  $ R.text t

secondaryButton
  :: forall t m.
     ( R.DomBuilder t m
     )
  => Text
  -> m ()
secondaryButton t = R'.elAttrClass
  "button"
  ("type" =: "button")
  "border border-brand-primary bg-white rounded text-blue-700 font-medium px-3 h-10"
  $ R.text t
