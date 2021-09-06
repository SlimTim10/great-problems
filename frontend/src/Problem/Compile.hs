module Problem.Compile
  ( widget
  , CompileResponse(..)
  ) where

import qualified Control.Lens as Lens
import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified GHC.Generics as Generics
import qualified Data.Aeson as JSON

import qualified JSDOM.Types
import qualified Language.Javascript.JSaddle as JS
import qualified Reflex.Dom.Core as R

-- Import unofficial patch
import qualified MyReflex.Dom.Xhr.FormData as R'

import qualified Widget.Button as Button
import qualified Problem.Options as Options
import qualified Problem.Figures as Figures
import Global

data CompileResponse = CompileResponse
  { errorIcemaker :: Text
  , errorLatex :: Text
  , pdfContent :: Text
  , pdfName :: Text
  , terminalOutput :: Text
  } deriving (Generics.Generic, Show)

instance JSON.FromJSON CompileResponse where
  parseJSON = JSON.genericParseJSON opts . jsonLower
    where opts = JSON.defaultOptions { JSON.fieldLabelModifier = map Char.toLower }

jsonLower :: JSON.Value -> JSON.Value
jsonLower (JSON.Object o) = JSON.Object . HM.fromList . map lowerPair . HM.toList $ o
  where lowerPair (key, val) = (T.toLower key, val)
jsonLower x = x

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
  -> m (R.Dynamic t (Maybe CompileResponse, Bool))
widget options figures prbName editorContent = do
  compile :: R.Event t () <- Button.primarySmallClass' "Compile" "active:bg-blue-400"

  let allData :: R.Dynamic t (Options.Options, [Figures.FileWithName], Text, Text) = (\ops figs nm ec -> (ops, figs, nm, ec)) <$> options <*> figures <*> prbName <*> editorContent
  formData :: R.Event t [Map Text (R'.FormValue JSDOM.Types.File)] <- R.performEvent $ R.ffor (R.tag (R.current allData) compile) $ \(ops, figs, nm, ec) -> do
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
  response <- R.holdDyn Nothing $ R.decodeText . T.concat . map (maybe "" id) <$> results
  loading <- R.zipDynWith
    (\(x :: Integer) (y :: Integer) -> x > 0 && x > y)
    <$> R.count compile <*> R.count (R.updated response)
  return $ R.zipDyn response loading
  where
    formFile f = R'.FormValue_File (Figures.file f) (Just (Figures.name f))
    formBool True = "true"
    formBool False = "false"
