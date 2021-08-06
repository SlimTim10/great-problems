module Problem.PdfViewer
  ( widget
  ) where

import qualified Data.Text as Text

import qualified Reflex.Dom.Core as R

import qualified Problem.Convert as Convert
import Global

widget
  :: ( R.DomBuilder t m
     , R.PostBuild t m
     )
  => R.Dynamic t Text
  -> R.Dynamic t Bool
  -> R.Dynamic t (Maybe Convert.ConvertResponse)
  -> R.Dynamic t Bool
  -> m ()
widget pdfData loading convertResponse errorsToggle = do
  R.dyn_ $ switchView <$> pdfData <*> loading <*> convertResponse <*> errorsToggle

switchView
  :: R.DomBuilder t m
  => Text
  -> Bool
  -> Maybe Convert.ConvertResponse
  -> Bool
  -> m ()
switchView pdfData loading convertResponse errorsToggle
  | loading = R.text "Loading..."
  | errorsToggle = errorsWidget convertResponse
  | Text.null pdfData = R.text "Press convert to view PDF"
  | otherwise = R.elAttr "iframe" attrs $ R.blank
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

errorsWidget
  :: R.DomBuilder t m
  => Maybe Convert.ConvertResponse
  -> m ()
errorsWidget Nothing = R.text ""
errorsWidget (Just res) = R.elClass "div" "flex flex-col w-full h-full" $ do
  R.elClass "p" "flex-1 overflow-y-auto border-b-2" $
    R.text (Convert.errorIcemaker res)
  R.elClass "p" "flex-1 overflow-y-auto" $
    R.text (Convert.errorLatex res)
