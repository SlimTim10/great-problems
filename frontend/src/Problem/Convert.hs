module Problem.Convert
  ( widget
  ) where

import qualified Control.Lens as Lens

import qualified JSDOM.Types
import qualified JSDOM.FormData as FormData
import qualified Language.Javascript.JSaddle as JS
import qualified Reflex.Dom.Core as R

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
  formData :: R.Event t [(Text, R.FormValue JSDOM.Types.File)] <- R.performEvent $ R.ffor (R.tag (R.current allData) convert) $ \(ops, nm, ec) -> do
    let
      r = Types.random ops
      o = Types.output ops
      fs = Types.files ops
      formDataText :: [(Text, R.FormValue JSDOM.Types.File)] =
        [ ("prbText", R.FormValue_Text ec)
        , ("prbName", R.FormValue_Text nm)
        , ("random", R.FormValue_Text (if r then "true" else "false"))
        , ("outFlag", R.FormValue_Text o)
        , ("submit1", R.FormValue_Text "putDatabase") -- temporary
        ]
      formDataFiles :: [(Text, R.FormValue JSDOM.Types.File)] = map (\f -> ("multiplefiles", Types.formValue f)) fs
    let formData = formDataText ++ formDataFiles
    return formData
  
  responses :: R.Event t R.XhrResponse <- postForm "https://icewire.ca/uploadprb" formData
  let results :: R.Event t (Maybe Text) = Lens.view R.xhrResponse_responseText <$> responses
  R.el "div" $ do
    result :: R.Dynamic t Text <- R.holdDyn "" $ maybe "" id <$> results
    return $ R.decodeText <$> result

postForm
  :: ( JSDOM.Types.IsBlob blob
     , R.HasJSContext (R.Performable m)
     , JS.MonadJSM (R.Performable m)
     , R.PerformEvent t m
     , R.TriggerEvent t m
     , Traversable f
     )
  => Text
  -> R.Event t (f (Text, R.FormValue blob))
  -> m (R.Event t R.XhrResponse)
postForm url payload = do
  R.performMkRequestAsync $ R.ffor payload $ \u -> JS.liftJSM $ do
    fd <- FormData.newFormData Nothing
    forM_ u $ \(k, v) -> case v of
      R.FormValue_Text t -> FormData.append fd k t
      R.FormValue_File b fn -> FormData.appendBlob fd k b fn
    return $ R.xhrRequest "POST" url $ R.def & R.xhrRequestConfig_sendData .~ fd
