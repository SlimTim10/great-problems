module Problem.PdfViewer
  ( widget
  ) where

import Common.Lib.Prelude

import qualified Data.Text as Text
import qualified Reflex.Dom.Core as R

import qualified Common.Api.Compile as Compile

widget
  :: forall t m.
     ( R.DomBuilder t m
     , R.PostBuild t m
     )
  => R.Dynamic t (Maybe Compile.Response)
  -> R.Dynamic t Bool
  -> R.Dynamic t Bool
  -> m ()
widget compileResponse loading errorsToggle = do
  let pdfData :: R.Dynamic t Text = maybe "" Compile.resPdfContents <$> compileResponse
  R.dyn_ $ switchView <$> pdfData <*> loading <*> compileResponse <*> errorsToggle

switchView
  :: R.DomBuilder t m
  => Text
  -> Bool
  -> Maybe Compile.Response
  -> Bool
  -> m ()
switchView pdfData loading compileResponse errorsToggle
  | loading = loadingWidget
  | errorsToggle = errorsWidget compileResponse
  | Text.null pdfData = R.text "Press compile to view PDF"
  | otherwise = R.elAttr "iframe" attrs $ do
      R.el "p" $ R.text "This browser does not support PDFs. Please download the PDF to view it:"
      R.elAttr "a"
        ( "href" =: pdfObjectSrc
          <> "download" =: "Calculus Demo Problem"
          <> "title" =: "Download PDF document"
        )
        $ R.text "Download PDF"
  where
    attrs :: Map Text Text
    attrs = (
      "src" =: pdfObjectSrc
      <> "height" =: "100%"
      <> "width" =: "100%"
      <> "type" =: "application/pdf"
      <> "title" =: "pdf"
      )
    pdfObjectSrc = "data:application/pdf;base64," <> pdfData

errorsWidget
  :: R.DomBuilder t m
  => Maybe Compile.Response
  -> m ()
errorsWidget Nothing = R.text ""
errorsWidget (Just res) = R.elClass "div" "flex flex-col w-full h-full" $ do
  R.elClass "p" "flex-1 overflow-y-auto border-b-2" $
    R.text $ Compile.resErrorIcemaker res
  R.elClass "p" "flex-1 overflow-y-auto" $
    R.text $ Compile.resErrorLatex res

loadingWidget
  :: R.DomBuilder t m
  => m ()
loadingWidget = R.elClass "div" "flex w-full h-full items-center justify-center" $ do
  R.elAttr "img" ("src" =: "/static/spinner.svg" <> "alt" =: "loading") $ R.blank
