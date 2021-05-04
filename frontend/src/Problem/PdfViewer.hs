{-# LANGUAGE OverloadedStrings #-}

module Problem.PdfViewer
  ( widget
  ) where

import qualified Reflex.Dom.Core as FRP

import Global

widget
  :: ( FRP.DomBuilder t m
     , FRP.PostBuild t m
     )
  => FRP.Dynamic t Text
  -> m ()
widget pdfData = FRP.el "div" $ do
  FRP.elDynAttr "iframe" (attrs <$> pdfData) $ FRP.blank
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
