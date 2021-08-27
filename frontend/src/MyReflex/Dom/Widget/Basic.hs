module MyReflex.Dom.Widget.Basic
  ( elAttrClass
  , elAttrClass'
  ) where

import qualified Reflex.Dom.Core as R

import Global

elAttrClass
  :: forall t m a.
     R.DomBuilder t m
  => Text
  -> Map Text Text
  -> Text
  -> m a
  -> m a
elAttrClass elementTag attrs c child = R.elAttr elementTag (attrs <> "class" =: c) child

elAttrClass'
  :: forall t m a.
     R.DomBuilder t m
  => Text
  -> Map Text Text
  -> Text
  -> m a
  -> m (R.Element R.EventResult (R.DomBuilderSpace m) t, a)
elAttrClass' elementTag attrs c child = R.elAttr' elementTag (attrs <> "class" =: c) child
