module Problem.PdfViewer
  ( widget
  ) where

import qualified Data.Text as Text

import qualified Reflex.Dom.Core as R

import Global

widget
  :: ( R.DomBuilder t m
     , R.PostBuild t m
     )
  => R.Dynamic t Text
  -> R.Dynamic t Bool
  -> m ()
widget pdfData loading = do
  R.dyn_ $ switchView <$> pdfData <*> loading

switchView :: R.DomBuilder t m => Text -> Bool -> m ()
switchView pdfData loading = case (Text.null pdfData, loading) of
  (_, True) -> R.text "Loading..."
  (True, False) -> R.text "Press convert to view PDF"
  (False, False) -> do
    R.elAttr "iframe" attrs $ R.blank
  where
    attrs :: Map Text Text
    attrs = (
      "src" =: (pdfObjectSrc <> pdfData)
      <> "height" =: "100%"
      <> "width" =: "100%"
      <> "type" =: "application/pdf"
      <> "title" =: "pdf"
      )
    pdfObjectSrc = "data:application/pdf;base64,"
