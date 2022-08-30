module Problem.PdfViewer
  ( widget
  ) where

import qualified Data.ByteString.Base64 as B64
import qualified Language.Javascript.JSaddle as JS
import qualified Obelisk.Generated.Static as Ob
import qualified Reflex.Dom.Core as R

import Common.Lib.Prelude
import qualified Frontend.Lib.Util as Util
import qualified Common.Api.Compile as Compile
import qualified Common.Api.Error as Error

widget
  :: forall t m.
     ( R.DomBuilder t m
     , R.PostBuild t m
     , R.PerformEvent t m
     , JS.MonadJSM (R.Performable m)
     , R.MonadHold t m
     )
  => R.Dynamic t (Maybe (Either Error.Error Text))
  -> R.Dynamic t Bool
  -> R.Dynamic t Bool
  -> m ()
widget compileResponse loading errorsToggle = do
  R.el "p" $ R.text "Not implemented"
  -- let pdfData :: R.Dynamic t (Maybe Text) = fmap Compile.resPdfContents <$> compileResponse
  -- pdfUrl :: R.Event t (Maybe Text) <- R.performEvent $ R.ffor (R.updated pdfData) $ \case
  --   Nothing -> return Nothing
  --   Just pdfData' -> case (B64.decode . cs) pdfData' of
  --     Left _ -> return Nothing
  --     Right bs -> do
  --       blobUrl <- Util.createObjectURL bs
  --       return . Just $ Ob.static @"pdf.js/web/viewer.html" <> "?file=" <> blobUrl
  -- pdfUrl' :: R.Dynamic t (Maybe Text) <- R.holdDyn Nothing pdfUrl
  -- R.dyn_ $ switchView <$> pdfUrl' <*> loading <*> compileResponse <*> errorsToggle

-- switchView
--   :: R.DomBuilder t m
--   => Maybe Text
--   -> Bool
--   -> Maybe Compile.Response
--   -> Bool
--   -> m ()
-- switchView pdfUrl loading compileResponse errorsToggle
--   | loading = loadingWidget
--   | errorsToggle = errorsWidget compileResponse
--   | isNothing pdfUrl = R.text "Press compile to view PDF"
--   | otherwise = R.elAttr "iframe" attrs $ do
--       R.el "p" $ R.text "This browser does not support PDFs. Please download the PDF to view it:"
--       R.elAttr "a"
--         ( "href" =: fromJust pdfUrl
--           <> "download" =: "Calculus Demo Problem"
--           <> "title" =: "Download PDF document"
--         )
--         $ R.text "Download PDF"
--   where
--     attrs :: Map Text Text
--     attrs = (
--       "src" =: fromJust pdfUrl
--       <> "height" =: "100%"
--       <> "width" =: "100%"
--       <> "type" =: "application/pdf"
--       <> "title" =: "pdf"
--       )

-- errorsWidget
--   :: R.DomBuilder t m
--   => Maybe Compile.Response
--   -> m ()
-- errorsWidget Nothing = R.text ""
-- errorsWidget (Just res) = R.elClass "div" "flex flex-col w-full h-full" $ do
--   R.elClass "p" "flex-1 overflow-y-auto border-b-2" $
--     R.text $ Compile.resErrorProblem2tex res
--   R.elClass "p" "flex-1 overflow-y-auto" $
--     R.text $ Compile.resErrorLatex res

-- loadingWidget
--   :: R.DomBuilder t m
--   => m ()
-- loadingWidget = R.elClass "div" "flex w-full h-full items-center justify-center" $ do
--   R.elAttr "img" ("src" =: Ob.static @"pdf_spinner.svg" <> "alt" =: "loading") $ R.blank
