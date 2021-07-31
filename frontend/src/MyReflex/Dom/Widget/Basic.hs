module MyReflex.Dom.Widget.Basic
  ( elAttrClass
  ) where

import qualified Reflex.Dom.Core as R

import Global

elAttrClass :: forall t m a. R.DomBuilder t m => Text -> Map Text Text -> Text -> m a -> m a
elAttrClass elementTag attrs c child = R.elAttr elementTag (attrs <> "class" =: c) child
