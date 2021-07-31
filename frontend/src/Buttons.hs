module Buttons
  ( primary
  , secondary
  ) where

import qualified Reflex.Dom.Core as R
import qualified MyReflex.Dom.Widget.Basic as R'

import Global

primary
  :: forall t m.
     ( R.DomBuilder t m
     )
  => Text
  -> m ()
primary t = R'.elAttrClass
  "button"
  ("type" =: "button")
  "bg-brand-primary rounded text-white font-medium px-3 h-10 mr-3"
  $ R.text t

secondary
  :: forall t m.
     ( R.DomBuilder t m
     )
  => Text
  -> m ()
secondary t = R'.elAttrClass
  "button"
  ("type" =: "button")
  "border border-brand-primary bg-white rounded text-blue-700 font-medium px-3 h-10"
  $ R.text t
