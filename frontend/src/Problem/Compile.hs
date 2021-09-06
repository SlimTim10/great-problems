module Problem.Compile
  ( widget
  ) where

import qualified Control.Lens as Lens
import qualified Data.Text as T
import qualified JSDOM.Types
import qualified Language.Javascript.JSaddle as JS
import qualified Reflex.Dom.Core as R
-- Import patch
import qualified MyReflex.Dom.Xhr.FormData as R'

import qualified Common.Compile as Compile
import qualified Common.File as File
import qualified Widget.Button as Button
import Global

widget
  :: forall t m.
     ( R.DomBuilder t m
     , R.MonadHold t m
     , JS.MonadJSM (R.Performable m)
     , R.HasJSContext (R.Performable m)
     , R.PerformEvent t m
     , R.TriggerEvent t m
     , MonadFix m
     )
  => R.Dynamic t Compile.CompileRequest
  -> m (R.Dynamic t (Maybe Compile.CompileResponse, Bool))
widget compileRequest = do
  compile :: R.Event t () <- Button.primarySmallClass' "Compile" "active:bg-blue-400"

  formData :: R.Event t [Map Text (R'.FormValue JSDOM.Types.File)] <- R.performEvent $ R.ffor (R.tagPromptlyDyn compileRequest compile) $ \cr -> do
    let
      formDataText :: Map Text (R'.FormValue JSDOM.Types.File) = (
        "prbText" =: R'.FormValue_Text (Compile.prbText cr)
        <> "prbName" =: R'.FormValue_Text (Compile.prbName cr)
        <> "random" =: R'.FormValue_Text (formBool . Compile.randomizeVariables $ cr)
        <> "outFlag" =: R'.FormValue_Text (cs . show . Compile.outputOption $ cr)
        <> "submit1" =: R'.FormValue_Text "putDatabase" -- temporary
        <> "multiplefiles" =: R'.FormValue_List (map formFile . Compile.figures $ cr)
        )
    return [formDataText]
  
  responses :: R.Event t [R.XhrResponse] <- R'.postForms "https://icewire.ca/uploadprb" formData
  let results :: R.Event t [Maybe Text] = map (Lens.view R.xhrResponse_responseText) <$> responses
  response <- R.holdDyn Nothing $ R.decodeText . T.concat . map (maybe "" id) <$> results
  loading <- R.zipDynWith
    (\(x :: Integer) (y :: Integer) -> x > 0 && x > y)
    <$> R.count compile <*> R.count (R.updated response)
  return $ R.zipDyn response loading
  where
    formFile f = R'.FormValue_File (File.file f) (Just (File.name f))
    formBool True = "true"
    formBool False = "false"
