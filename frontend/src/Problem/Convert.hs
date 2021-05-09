module Problem.Convert
  ( widget
  ) where

import qualified Control.Lens as Lens
import qualified Data.Text as T

import qualified JSDOM.Types
import qualified Language.Javascript.JSaddle as JS
import qualified Reflex.Dom.Core as R

-- Import unofficial patch
import qualified Xhr.FormData as R'

import qualified Problem.Types as Types
import Global

widget
  :: forall t m.
     ( R.DomBuilder t m
     , R.MonadHold t m
     , JS.MonadJSM (R.Performable m)
     , R.HasJSContext (R.Performable m)
     , R.PerformEvent t m
     , R.TriggerEvent t m
     )
  => R.Dynamic t Types.Options
  -> R.Dynamic t Text
  -> R.Dynamic t Text
  -> m (R.Dynamic t (Maybe Types.ConvertResponse))
widget options prbName editorContent = R.el "div" $ do
  convert :: R.Event t () <- R.button "Convert"

  let allData :: R.Dynamic t (Types.Options, Text, Text) = (\ops nm ec -> (ops, nm, ec)) <$> options <*> prbName <*> editorContent
  formData :: R.Event t [Map Text (R'.FormValue JSDOM.Types.File)] <- R.performEvent $ R.ffor (R.tag (R.current allData) convert) $ \(ops, nm, ec) -> do
    let
      r = Types.random ops
      o = Types.output ops
      fs = Types.files ops
      formDataText :: Map Text (R'.FormValue JSDOM.Types.File) = (
        "prbText" =: R'.FormValue_Text ec
        <> "prbName" =: R'.FormValue_Text nm
        <> "random" =: R'.FormValue_Text (formBool r)
        <> "outFlag" =: R'.FormValue_Text o
        <> "submit1" =: R'.FormValue_Text "putDatabase" -- temporary
        <> "multiplefiles" =: R'.FormValue_List (map formFile fs)
        )
    return [formDataText]
  
  responses :: R.Event t [R.XhrResponse] <- R'.postForms "https://icewire.ca/uploadprb" formData
  let results :: R.Event t [Maybe Text] = map (Lens.view R.xhrResponse_responseText) <$> responses
  R.el "div" $ do
    result :: R.Dynamic t Text <- R.holdDyn "" $ T.concat . map (maybe "" id) <$> results
    return $ R.decodeText <$> result
  where
    formFile f = R'.FormValue_File (Types.file f) (Just (Types.name f))
    formBool True = "true"
    formBool False = "false"
