module Problem.DownloadPrb
  ( widget
  ) where

import qualified Data.Text.Encoding as T

import qualified JSDOM.Types
import qualified JSDOM.URL as URL
import qualified JSDOM.Blob as Blob
import qualified Language.Javascript.JSaddle as JS
import qualified Foreign.JavaScript.Utils as Utils
import qualified Reflex.Dom.Core as R

import Global

createObjectURL
  :: ( R.DomBuilder t m
     , R.PerformEvent t m
     , JS.MonadJSM (R.Performable m)
     )
  => R.Dynamic t Text
  -> m (R.Event t Text)
createObjectURL contentD = do
  R.performEvent $ R.ffor (R.updated contentD) $ \content -> do
    JS.liftJSM $ do
      t <- Utils.bsToArrayBuffer $ T.encodeUtf8 content
      o <- JS.obj ^. JS.jss ("type" :: Text) ("text/plain" :: Text)
      options <- JSDOM.Types.BlobPropertyBag <$> JS.toJSVal o
      blob <- Blob.newBlob [t] (Just options)
      URL.createObjectURL blob

widget
  :: forall t m.
     ( R.DomBuilder t m
     , JS.MonadJSM (R.Performable m)
     , R.PerformEvent t m
     , R.PostBuild t m
     , R.MonadHold t m
     )
  => R.Dynamic t Text
  -> R.Dynamic t Text
  -> m ()
widget prbNameD prbContentD = do
  hrefD <- R.holdDyn "" =<< createObjectURL prbContentD
  R.elDynAttr "a" (attrs <$> prbNameD <*> hrefD) (R.text "Download PRB")
  where
    attrs :: Text -> Text -> Map Text Text
    attrs prbName href = (
      "href" =: href
      <> "download" =: (prbName <> ".prb")
      )
