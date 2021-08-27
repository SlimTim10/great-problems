module Buttons
  ( primary
  , secondary
  , primary'
  , secondary'
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
  "bg-brand-primary rounded text-white font-medium px-3 h-10"
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
  "border border-brand-primary bg-transparent rounded text-blue-700 font-medium px-3 h-10"
  $ R.text t

primary'
  :: forall t m.
     ( R.DomBuilder t m
     )
  => Text
  -> m (R.Event t ())
primary' t = do
  (e, _) <- R'.elAttrClass'
    "button"
    ("type" =: "button")
    "bg-brand-primary rounded text-white font-medium px-3 h-10"
    $ R.text t
  return $ R.domEvent R.Click e

secondary'
  :: forall t m.
     ( R.DomBuilder t m
     )
  => Text
  -> m (R.Event t ())
secondary' t = do
  (e, _) <- R'.elAttrClass'
    "button"
    ("type" =: "button")
    "border border-brand-primary bg-transparent rounded text-blue-700 font-medium px-3 h-10"
    $ R.text t
  return $ R.domEvent R.Click e
