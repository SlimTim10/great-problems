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
import qualified Problem.Options as Options
import qualified Problem.Figures as Figures
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
  => R.Dynamic t Options.Options
  -> R.Dynamic t [Figures.FileWithName]
  -> R.Dynamic t Text
  -> R.Dynamic t Text
  -> m (R.Dynamic t (Maybe Types.ConvertResponse, Bool))
widget options figures prbName editorContent = do
  convert :: R.Event t () <- R.button "Convert"

  let allData :: R.Dynamic t (Options.Options, [Figures.FileWithName], Text, Text) = (\ops figs nm ec -> (ops, figs, nm, ec)) <$> options <*> figures <*> prbName <*> editorContent
  formData :: R.Event t [Map Text (R'.FormValue JSDOM.Types.File)] <- R.performEvent $ R.ffor (R.tag (R.current allData) convert) $ \(ops, figs, nm, ec) -> do
    let
      r = Options.random ops
      o = Options.output ops
      formDataText :: Map Text (R'.FormValue JSDOM.Types.File) = (
        "prbText" =: R'.FormValue_Text ec
        <> "prbName" =: R'.FormValue_Text nm
        <> "random" =: R'.FormValue_Text (formBool r)
        <> "outFlag" =: R'.FormValue_Text o
        <> "submit1" =: R'.FormValue_Text "putDatabase" -- temporary
        <> "multiplefiles" =: R'.FormValue_List (map formFile figs)
        )
    return [formDataText]
  
  responses :: R.Event t [R.XhrResponse] <- R'.postForms "https://icewire.ca/uploadprb" formData
  let results :: R.Event t [Maybe Text] = map (Lens.view R.xhrResponse_responseText) <$> responses
  R.el "div" $ do
    response <- R.holdDyn Nothing $ R.decodeText . T.concat . map (maybe "" id) <$> results
    loading <- R.zipDynWith
      (\(x :: Integer) (y :: Integer) -> x > 0 && x > y)
      <$> R.count convert <*> R.count (R.updated response)
    return $ R.zipDyn response loading
  where
    formFile f = R'.FormValue_File (Figures.file f) (Just (Figures.name f))
    formBool True = "true"
    formBool False = "false"
