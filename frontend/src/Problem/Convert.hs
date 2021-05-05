module Problem.Convert
  ( widget
  ) where

import qualified Control.Lens as Lens
import qualified Data.Text as T
import qualified Data.Map as Map

import qualified JSDOM.Types
import qualified JSDOM.FileReader as FileReader
import qualified JSDOM.EventM as EventM
import qualified Language.Javascript.JSaddle as JS
import qualified Reflex.Dom.Core as R

import qualified Problem.Types as Types
import Global
import Util

widget
  :: forall t m.
     ( R.DomBuilder t m
     , R.MonadHold t m
     , JS.MonadJSM (R.Performable m)
     , R.HasJSContext (R.Performable m)
     , R.PerformEvent t m
     , R.TriggerEvent t m
     , JS.MonadJSM m
     )
  => Types.Options t
  -> R.Dynamic t Text
  -> R.Dynamic t Text
  -> m (R.Dynamic t (Maybe Types.ConvertResponse))
widget options prbName editorContent = R.el "div" $ do
  evConvert :: R.Event t () <- R.button "Convert"

  let evFilesAndConvert = R.attach (R.current $ Types.files options) evConvert
  R.widgetHold_ R.blank . R.ffor evFilesAndConvert $ \(fs, _) -> do
    let fs' :: [JSDOM.Types.File] = map Types.file fs
    mapM_ printFileContents fs'
  
  let evFormData :: R.Event t [Map Text (R.FormValue JSDOM.Types.File)] = R.pushAlways (const buildFormData) evConvert
  responses :: R.Event t [R.XhrResponse] <- R.postForms "https://icewire.ca/uploadprb" evFormData
  let results :: R.Event t [Maybe Text] = map (Lens.view R.xhrResponse_responseText) <$> responses
  R.el "div" $ do
    result :: R.Dynamic t Text <- R.holdDyn "" $ T.concat . map (maybe "" id) <$> results
    return $ R.decodeText <$> result
  where
    buildFormData :: R.PushM t [Map Text (R.FormValue JSDOM.Types.File)]
    buildFormData = do
      r <- R.sample . R.current $ Types.random options
      o <- R.sample . R.current $ Types.output options
      fs <- R.sample . R.current $ Types.files options
      nm <- R.sample . R.current $ prbName
      t <- R.sample . R.current $ editorContent
      let
        formDataText :: Map Text (R.FormValue JSDOM.Types.File) = (
          "prbText" =: R.FormValue_Text t
          <> "prbName" =: R.FormValue_Text nm
          <> "random" =: R.FormValue_Text (if r then "true" else "false")
          <> "outFlag" =: R.FormValue_Text o
          <> "submit1" =: R.FormValue_Text "putDatabase" -- temporary
          )
        formDataFiles :: Map Text (R.FormValue JSDOM.Types.File) = Map.fromList $ flip map fs $ \f ->
          let
            fn = Types.name f
            fval = R.FormValue_File (Types.file f) (Just fn)
          in (fn, fval)
      let formData = Map.unions [formDataText, formDataFiles]
      return [formData]

    printFileContents :: JSDOM.Types.File -> m ()
    printFileContents f = do
      fileReader <- JS.liftJSM FileReader.newFileReader
      FileReader.readAsText fileReader (Just f) (Just "utf8" :: Maybe Text)
      e :: R.Event t (Maybe Text) <- R.wrapDomEvent fileReader (`EventM.on` FileReader.load) . JS.liftJSM $ do
        consoleLog ("fileReader onload" :: Text)
        v <- FileReader.getResult fileReader
        (JS.fromJSVal <=< JS.toJSVal) v
      evFileText :: R.Event t Text <- return (R.fmapMaybe id e)
      R.widgetHold_ R.blank . R.ffor evFileText $ \fileText -> do
        consoleLog ("fileText:" :: Text)
        consoleLog fileText
