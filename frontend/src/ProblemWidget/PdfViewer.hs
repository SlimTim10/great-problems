{-# LANGUAGE OverloadedStrings #-}

module ProblemWidget.PdfViewer
  ( pdfViewerWidget
  ) where

import Data.Text (Text)
import Data.Map (Map)

import Reflex.Dom.Core

pdfViewerWidget
  :: ( DomBuilder t m
     , PostBuild t m
     )
  => Dynamic t Text
  -> m ()
pdfViewerWidget pdfData = el "div" $ do
  elDynAttr "iframe" (attrs <$> pdfData) $ blank
  where
    attrs :: Text -> Map Text Text
    attrs pdfData' = (
      "src" =: (pdfObjectSrc <> pdfData')
      <> "height" =: "100%"
      <> "width" =: "100%"
      <> "type" =: "application/pdf"
      <> "title" =: "pdf"
      )
    pdfObjectSrc = "data:application/pdf;base64,"
