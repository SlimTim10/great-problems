module Problem.PdfViewer
  ( widget
  ) where

import qualified Reflex.Dom.Core as R

import Global

widget
  :: ( R.DomBuilder t m
     , R.PostBuild t m
     )
  => R.Dynamic t Text
  -> m ()
widget pdfData = do
  R.elDynAttr "iframe" (attrs <$> pdfData) $ R.blank
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
