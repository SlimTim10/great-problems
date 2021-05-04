{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module ProblemWidget
  ( problemWidget
  ) where

import Control.Monad.Fix
import Data.Text (Text)

import Language.Javascript.JSaddle (MonadJSM)

import Reflex.Dom.Core

import ProblemWidget.Types
import ProblemWidget.Options
import ProblemWidget.Convert
import ProblemWidget.Editor
import ProblemWidget.PdfViewer

problemWidget
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     , MonadJSM m
     , MonadJSM (Performable m)
     , HasJSContext (Performable m)
     , PerformEvent t m
     , TriggerEvent t m
     )
  => m ()
problemWidget = do
  options :: Options t <- optionsWidget
  (evUploadPrb, evDownloadPrv, prbName) <- el "div" $ do
    evUploadPrb :: Event t () <- button "Upload PRB"
    evDownloadPrb :: Event t () <- button "Download PRB"
    prbNameEl <- inputElement $ def
      & inputElementConfig_initialValue .~ "untitled"
    return (evUploadPrb, evDownloadPrb, value prbNameEl)
  editorContent :: Dynamic t Text <- editorWidget
  convertResponse <- convertWidget options prbName editorContent
  let pdfData = maybe "" pdfContent <$> convertResponse
  pdfViewerWidget pdfData
