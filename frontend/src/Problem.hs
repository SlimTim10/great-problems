module Problem
  ( widget
  ) where

import qualified Control.Monad.Fix as Fix

import qualified Language.Javascript.JSaddle as JS
import qualified Reflex.Dom.Core as R

import qualified Problem.Options as Options
import qualified Problem.Figures as Figures
import qualified Problem.Convert as Convert
import qualified Problem.Editor as Editor
import qualified Problem.PdfViewer as PdfViewer
import qualified Problem.UploadPrb as UploadPrb
import qualified Problem.DownloadPrb as DownloadPrb
import qualified Problem.ErrorsToggle as ErrorsToggle
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
    
    (options, figures) <- R.elClass "div" "flex-none w-56 flex flex-col gap-4" $ do
      options <- R.elClass "div" "border-2 border-gray-300" $ Options.widget
      figures <- R.elClass "div" "h-full border-2 border-gray-300" $ Figures.widget
      return (options, figures)
      
    R.elClass "div" "flex-1 h-full flex flex-col" $ mdo
      
      (uploadPrb, convertResponse, loading, errorsToggle) <- R.elClass "div" "bg-gray-100 flex justify-between" $ mdo
        uploadPrb <- UploadPrb.widget
        DownloadPrb.widget prbName editorContent
        prbName <- prbNameWidget
        (convertResponse, loading) <- Convert.widget options figures prbName editorContent
          <&> R.splitDynPure
        errorsToggle <- ErrorsToggle.widget convertResponse loading
        return (uploadPrb, convertResponse, loading, errorsToggle)
        
      editorContent <- R.elClass "div" "h-full flex" $ mdo
        editorContent <- R.elClass "div" "h-full flex-1"$ Editor.widget uploadPrb
        let pdfData = maybe "" Convert.pdfContent <$> convertResponse
        R.elClass "div" "flex-1" $ PdfViewer.widget pdfData loading convertResponse errorsToggle
        return editorContent
        
      return ()

prbNameWidget :: (R.DomBuilder t m) => m (R.Dynamic t Text)
prbNameWidget = do
  R.value
  <$> R.inputElement (R.def & R.inputElementConfig_initialValue .~ "untitled")
