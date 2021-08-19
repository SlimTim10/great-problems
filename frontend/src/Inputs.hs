module Inputs
  ( textClass
  , emailClass
  , passwordClass
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
