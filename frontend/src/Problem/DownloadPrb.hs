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
createObjectURL content = do
  R.performEvent $ R.ffor (R.updated content) $ \c -> do
    JS.liftJSM $ do
      t <- Utils.bsToArrayBuffer $ T.encodeUtf8 c
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
widget prbName prbContent = do
  href <- R.holdDyn "" =<< createObjectURL prbContent
  R.elDynAttr "a" (attrs <$> prbName <*> href) (R.text "Download PRB")
  where
    attrs :: Text -> Text -> Map Text Text
    attrs nm h = (
      "href" =: h
      <> "download" =: (nm <> ".prb")
      )
