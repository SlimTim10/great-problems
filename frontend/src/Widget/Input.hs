module Widget.Input
  ( textClass
  , emailClass
  , passwordClass
  , textAreaClass
  , dropdownClass
  ) where

import qualified Reflex.Dom.Core as R

import Global

textClass
  :: forall t m.
     ( R.DomBuilder t m
     )
  => Text -- ^ Style
  -> m (R.Dynamic t Text)
textClass c = fmap R.value $ R.inputElement $
  R.def & R.inputElementConfig_elementConfig . R.elementConfig_initialAttributes .~
  ( "type" =: "text"
    <> "class" =: c
  )

emailClass
  :: forall t m.
     ( R.DomBuilder t m
     )
  => Text -- ^ Style
  -> m (R.Dynamic t Text)
emailClass c = fmap R.value $ R.inputElement $
  R.def & R.inputElementConfig_elementConfig . R.elementConfig_initialAttributes .~
  ( "type" =: "email"
    <> "class" =: c
  )

passwordClass
  :: forall t m.
     ( R.DomBuilder t m
     )
  => Text -- ^ Style
  -> m (R.Dynamic t Text)
passwordClass c = fmap R.value $ R.inputElement $
  R.def & R.inputElementConfig_elementConfig . R.elementConfig_initialAttributes .~
  ( "type" =: "password"
    <> "class" =: c
  )

textAreaClass
  :: forall t m.
     ( R.DomBuilder t m
     )
  => Text -- ^ Style
  -> m (R.Dynamic t Text)
textAreaClass c = fmap R.value $ R.textAreaElement $
  R.def & R.textAreaElementConfig_elementConfig . R.elementConfig_initialAttributes .~
  ("class" =: c)

dropdownClass
  :: forall t m k.
     ( R.DomBuilder t m
     , MonadFix m
     , R.MonadHold t m
     , R.PostBuild t m
     , Ord k
     )
  => Text -- ^ Style
  -> k
  -> R.Dynamic t (Map k Text)
  -> m (R.Dynamic t k) 
dropdownClass c k0 options = do
  fmap R.value $ R.dropdown k0 options $
    R.def & R.dropdownConfig_attributes .~
    R.constDyn ("class" =: c)
