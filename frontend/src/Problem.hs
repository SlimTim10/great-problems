module Problem
  ( widget
  ) where

import qualified Control.Monad.Fix as Fix

import qualified Language.Javascript.JSaddle as JS
import qualified Reflex.Dom.Core as R

import qualified Problem.Types as Types
import qualified Problem.Options as Options
import qualified Problem.Convert as Convert
import qualified Problem.Editor as Editor
import qualified Problem.PdfViewer as PdfViewer
import qualified Problem.UploadPrb as UploadPrb
import Global

widget
  :: ( R.DomBuilder t m
     , R.PostBuild t m
     , R.MonadHold t m
     , Fix.MonadFix m
     , JS.MonadJSM m
     , JS.MonadJSM (R.Performable m)
     , R.HasJSContext (R.Performable m)
     , R.PerformEvent t m
     , R.TriggerEvent t m
     )
  => m ()
widget = do
  options :: Types.Options t <- Options.widget
  (evUploadPrb, evDownloadPrv, prbName) <- R.el "div" $ do
    evUploadPrb :: R.Event t () <- R.button "Upload PRB"
    evDownloadPrb :: R.Event t () <- R.button "Download PRB"
    prbNameEl <- R.inputElement $ R.def
      & R.inputElementConfig_initialValue .~ "untitled"
    return (evUploadPrb, evDownloadPrb, R.value prbNameEl)
  -- let v :: R.Event t Text = R.updated . R.constDyn $ "test" -- testing
  -- let d = R.constDyn "test"
  -- let v' = R.pushAlways (const $ R.sample . R.current $ d) evUploadPrb
  editorContent :: R.Dynamic t Text <- Editor.widget =<< UploadPrb.widget
  convertResponse <- Convert.widget options prbName editorContent
  let pdfData = maybe "" Types.pdfContent <$> convertResponse
  PdfViewer.widget pdfData
