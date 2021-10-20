module Problem.DownloadPrb
  ( widget
  ) where

import Frontend.Lib.Prelude

import qualified Data.Text.Encoding as T
import qualified GHCJS.DOM.Types
import qualified GHCJS.DOM.URL as URL
import qualified GHCJS.DOM.Blob as Blob
import qualified Language.Javascript.JSaddle as JS
import qualified Foreign.JavaScript.Utils as Utils
import qualified Reflex.Dom.Core as R

import qualified Widget.Button as Button

createObjectURL
  :: ( R.DomBuilder t m
     , R.PerformEvent t m
     , JS.MonadJSM (R.Performable m)
     )
  => R.Dynamic t Text
  -> m (R.Event t Text)
createObjectURL contents = do
  R.performEvent $ R.ffor (R.updated contents) $ \c -> JS.liftJSM $ do
    t <- Utils.bsToArrayBuffer $ T.encodeUtf8 c
    o <- JS.obj ^. JS.jss ("type" :: Text) ("text/plain" :: Text)
    options <- GHCJS.DOM.Types.BlobPropertyBag <$> JS.toJSVal o
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
widget prbName prbContents = do
  href <- R.holdDyn "" =<< createObjectURL prbContents
  R.elDynAttr "a" (attrs <$> prbName <*> href) (Button.primarySmallClass "Download PRB" "active:bg-blue-400")
  where
    attrs :: Text -> Text -> Map Text Text
    attrs nm h = (
      "href" =: h
      <> "download" =: (nm <> ".prb")
      <> "class" =: "min-w-fit"
      )
