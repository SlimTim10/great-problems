module Problem.Convert
  ( widget
  ) where

import qualified Control.Lens as Lens
import qualified Data.Text as T

import qualified JSDOM.Types
import qualified JSDOM.FormData as FormData
import qualified Language.Javascript.JSaddle as JS
import qualified Reflex.Dom.Core as R

import qualified Problem.Types as Types
import Global

data FormValue blob
  = FormValue_Text Text
  | FormValue_File blob (Maybe Text) -- maybe filename
  | FormValue_List [FormValue blob]

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
  formData :: R.Event t [Map Text (FormValue JSDOM.Types.File)] <- R.performEvent $ R.ffor (R.tag (R.current allData) convert) $ \(ops, nm, ec) -> do
    let
      r = Types.random ops
      o = Types.output ops
      fs = Types.files ops
      formDataText :: Map Text (FormValue JSDOM.Types.File) = (
        "prbText" =: FormValue_Text ec
        <> "prbName" =: FormValue_Text nm
        <> "random" =: FormValue_Text (formBool r)
        <> "outFlag" =: FormValue_Text o
        <> "submit1" =: FormValue_Text "putDatabase" -- temporary
        <> "multiplefiles" =: FormValue_List (map formFile fs)
        )
    return [formDataText]
  
  responses :: R.Event t [R.XhrResponse] <- postForms "https://icewire.ca/uploadprb" formData
  let results :: R.Event t [Maybe Text] = map (Lens.view R.xhrResponse_responseText) <$> responses
  R.el "div" $ do
    result :: R.Dynamic t Text <- R.holdDyn "" $ T.concat . map (maybe "" id) <$> results
    return $ R.decodeText <$> result
  where
    formFile f = FormValue_File (Types.file f) (Just (Types.name f))
    formBool True = "true"
    formBool False = "false"

-- | Performs a POST request with the provided FormData payload
postForms
  :: ( JSDOM.Types.IsBlob blob, R.HasJSContext (R.Performable m), JS.MonadJSM (R.Performable m)
     , R.PerformEvent t m, R.TriggerEvent t m
     , Traversable f)
  => Text -- ^ The target url
  -> R.Event t (f (Map Text (FormValue blob))) -- ^ Maps of text keys and values that will be sent as "FormData"
  -> m (R.Event t (f R.XhrResponse))
postForms url payload = do
  R.performMkRequestsAsync $ R.ffor payload $ \fs -> for fs $ \u -> JS.liftJSM $ do
    fd <- FormData.newFormData Nothing
    iforM_ u $ \k v -> appendFormValue k v fd
    return $ R.xhrRequest "POST" url $ R.def & R.xhrRequestConfig_sendData .~ fd
  where
    appendFormValue k v fd = case v of
      FormValue_Text t -> FormData.append fd k t
      FormValue_File b fn -> FormData.appendBlob fd k b fn
      FormValue_List vs' -> forM_ vs' $ \v' -> appendFormValue k v' fd
