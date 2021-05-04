{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Problem
  ( widget
  ) where

import qualified Control.Monad.Fix as Fix

import qualified Language.Javascript.JSaddle as JS
import qualified Reflex.Dom.Core as Frp

import qualified Problem.Types as Types
import qualified Problem.Options as Options
import qualified Problem.Convert as Convert
import qualified Problem.Editor as Editor
import qualified Problem.PdfViewer as PdfViewer
import Global

widget
  :: ( Frp.DomBuilder t m
     , Frp.PostBuild t m
     , Frp.MonadHold t m
     , Fix.MonadFix m
     , JS.MonadJSM m
     , JS.MonadJSM (Frp.Performable m)
     , Frp.HasJSContext (Frp.Performable m)
     , Frp.PerformEvent t m
     , Frp.TriggerEvent t m
     )
  => m ()
widget = do
  options :: Types.Options t <- Options.widget
  (evUploadPrb, evDownloadPrv, prbName) <- Frp.el "div" $ do
    evUploadPrb :: Frp.Event t () <- Frp.button "Upload PRB"
    evDownloadPrb :: Frp.Event t () <- Frp.button "Download PRB"
    prbNameEl <- Frp.inputElement $ Frp.def
      & Frp.inputElementConfig_initialValue .~ "untitled"
    return (evUploadPrb, evDownloadPrb, Frp.value prbNameEl)
  editorContent :: Frp.Dynamic t Text <- Editor.widget
  convertResponse <- Convert.widget options prbName editorContent
  let pdfData = maybe "" Types.pdfContent <$> convertResponse
  PdfViewer.widget pdfData
