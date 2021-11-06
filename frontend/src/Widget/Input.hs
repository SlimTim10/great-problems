module Widget.Input
  ( textClass
  , rawTextClass
  , emailClass
  , rawEmailClass
  , passwordClass
  , rawPasswordClass
  , textAreaClass
  , textAreaClass'
  , dropdownClass
  , dropdownClass'
  , checkboxClass
  ) where

import Common.Lib.Prelude

import qualified Reflex.Dom.Core as R

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

rawTextClass
  :: forall t m.
     ( R.DomBuilder t m
     )
  => Text -- ^ Style
  -> m (R.InputElement R.EventResult (R.DomBuilderSpace m) t)
rawTextClass c = R.inputElement $
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

rawEmailClass
  :: forall t m.
     ( R.DomBuilder t m
     )
  => Text -- ^ Style
  -> m (R.InputElement R.EventResult (R.DomBuilderSpace m) t)
rawEmailClass c = R.inputElement $
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

rawPasswordClass
  :: forall t m.
     ( R.DomBuilder t m
     )
  => Text -- ^ Style
  -> m (R.InputElement R.EventResult (R.DomBuilderSpace m) t)
rawPasswordClass c = R.inputElement $
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
  R.def
  & R.textAreaElementConfig_elementConfig . R.elementConfig_initialAttributes .~
  ("class" =: c)

textAreaClass'
  :: forall t m.
     ( R.DomBuilder t m
     )
  => Text -- ^ Style
  -> R.Event t Text -- ^ Set value
  -> m (R.Dynamic t Text)
textAreaClass' c setValue = fmap R.value $ R.textAreaElement $
  R.def
  & R.textAreaElementConfig_elementConfig . R.elementConfig_initialAttributes .~ ("class" =: c)
  & R.textAreaElementConfig_setValue .~ setValue

dropdownClass
  :: forall t m k.
     ( R.DomBuilder t m
     , MonadFix m
     , R.MonadHold t m
     , R.PostBuild t m
     , Ord k
     )
  => Text -- ^ Style
  -> k -- ^ Default option selected
  -> R.Dynamic t (Map k Text) -- ^ Options
  -> m (R.Dynamic t k) 
dropdownClass c k0 options = do
  fmap R.value $ R.dropdown k0 options $
    R.def & R.dropdownConfig_attributes .~
    R.constDyn ("class" =: c)

dropdownClass'
  :: forall t m k.
     ( R.DomBuilder t m
     , MonadFix m
     , R.MonadHold t m
     , R.PostBuild t m
     , Ord k
     )
  => Text -- ^ Style
  -> k -- ^ Default option selected
  -> R.Dynamic t (Map k Text) -- ^ Options
  -> R.Event t k -- ^ Set value
  -> m (R.Dynamic t k) 
dropdownClass' c k0 options setValue = do
  fmap R.value $ R.dropdown k0 options $
    R.def
    & R.dropdownConfig_attributes .~ R.constDyn ("class" =: c)
    & R.dropdownConfig_setValue .~ setValue
    

checkboxClass
  :: forall t m.
     ( R.DomBuilder t m
     )
  => Text -- ^ Style for checkbox
  -> Text -- ^ Style for label
  -> Text -- ^ Label
  -> m (R.Dynamic t Bool)
checkboxClass cCheckbox cLabel label = do
  cb <- R.elClass "label" cLabel $ do
    cb1 <- R.inputElement
      $ R.def & R.initialAttributes .~
      ( "type" =: "checkbox"
        <> "class" =: cCheckbox
      )
    R.text label
    return cb1
  return $ R._inputElement_checked cb
