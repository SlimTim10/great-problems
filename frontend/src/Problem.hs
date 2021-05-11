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
import qualified Problem.DownloadPrb as DownloadPrb
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
  R.elClass "div" "flex-1 h-full flex gap-4" $ do
    options :: R.Dynamic t Options.Options <- R.elClass "div" "border-2 border-gray-300 flex-none w-56" $ do
      Options.widget
    R.elClass "div" "flex-1 h-full flex flex-col" $ mdo
      (uploadPrb, downloadPrb, convertResponse) <- R.elClass "div" "bg-gray-100" $ mdo
        uploadPrb <- UploadPrb.widget
        DownloadPrb.widget prbName editorContent
        prbName <- R.value <$> (R.inputElement $ R.def
          & R.inputElementConfig_initialValue .~ "untitled")
        convertResponse <- Convert.widget options prbName editorContent
        return (uploadPrb, downloadPrb, convertResponse)
      editorContent <- R.elClass "div" "h-full flex" $ mdo
        editorContent <- R.elClass "div" "h-full flex-1"$ Editor.widget uploadPrb
        let pdfData = maybe "" Types.pdfContent <$> convertResponse
        R.elClass "div" "flex-1" $ PdfViewer.widget pdfData
        return editorContent
      return ()
  return ()
