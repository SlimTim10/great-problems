{-# LANGUAGE PackageImports #-}
module Frontend.Lib.Util where

import Common.Lib.Prelude

import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Control.Lens as Lens
import qualified Data.Aeson as JSON
import qualified Control.Monad.IO.Class as IO
import qualified Web.Cookie as Cookie
import qualified Language.Javascript.JSaddle as JS
import qualified "ghcjs-dom" GHCJS.DOM.Document as DOM
import qualified GHCJS.DOM.Types
import qualified GHCJS.DOM.Blob
import qualified GHCJS.DOM.URL
import qualified Foreign.JavaScript.Utils as JSUtils
import qualified Reflex.Dom.Core as R
-- Import patch
import qualified MyReflex.Dom.Xhr.FormData as R'

import qualified Common.Api.User as User
import qualified Problem.FormFile as FormFile

consoleLog :: (JS.MonadJSM m, JS.ToJSVal v) => v -> m ()
consoleLog x = void $ JS.liftJSM $ do
  w <- JS.jsg ("console" :: Text)
  w ^. JS.js1 ("log" :: Text) x

-- | Generate a random 32-bit integer in JavaScript
random32 :: JS.MonadJSM m => m Integer
random32 = JS.liftJSM $ do
  x :: JS.JSVal <- JS.eval ("Math.floor(Math.random() * (2**32 - 1))" :: Text)
  floor <$> JS.valToNumber x

buttonDynClass
  :: ( R.DomBuilder t m
     , R.PostBuild t m
     )
  => Text
  -> R.Dynamic t Text
  -> m (R.Event t ())
buttonDynClass t c = do
  (e, _) <- R.elDynClass' "button" c $ R.text t
  return $ R.domEvent R.Click e

getOnload
  :: ( R.PostBuild t m
     , JSON.FromJSON a
     , JS.MonadJSM (R.Performable m)
     , IO.MonadIO m
     , R.PerformEvent t m
     , R.HasJSContext (R.Performable m)
     , R.TriggerEvent t m
     )
  => Text -> m (R.Event t (Maybe a))
getOnload url = do
  onload :: R.Event t () <- R.getPostBuild
  let endpoint :: R.Event t Text = R.tagPromptlyDyn (R.constDyn url) onload
  R.getAndDecode endpoint

dynFor
  :: ( R.Adjustable t m
     , R.NotReady t m
     , R.PostBuild t m
     )
  => R.Dynamic t a -> (a -> m a1) -> m ()
dynFor x = R.dyn_ . R.ffor x

-- | Get the current user from the cookie, if there is one
getCurrentUser
  :: ( JS.MonadJSM m
     , DOM.IsDocument (R.RawDocument (R.DomBuilderSpace m))
     , R.HasDocument m
     )
  => m (Maybe User.User)
getCurrentUser = do
  rawCookies :: Text <- DOM.getCookie =<< R.askDocument
  let cookies :: Cookie.Cookies = Cookie.parseCookies (cs rawCookies)
  return $ JSON.decode . cs =<< lookup "user" cookies

formFile :: FormFile.FormFile -> R'.FormValue GHCJS.DOM.Types.File
formFile f = R'.FormValue_File (FormFile.file f) (Just (FormFile.name f))

formBool :: Bool -> Text
formBool True = "true"
formBool False = "false"

-- | Make posting forms less awkward. Send a single map of params return the response as text.
postForm
  :: forall t m.
     ( R.MonadHold t m
     , R.HasJSContext (R.Performable m)
     , JS.MonadJSM (R.Performable m)
     , R.PerformEvent t m
     , R.TriggerEvent t m
     )
  => Text
  -> R.Event t (Map Text (R'.FormValue GHCJS.DOM.Types.File))
  -> m (R.Event t Text)
postForm url formData = do
  responses <- R'.postForms url (singleton <$> formData)
  let results :: R.Event t [Maybe Text] = map (Lens.view R.xhrResponse_responseText) <$> responses
  return $ T.concat . map (maybe "" id) <$> results

-- | Create a new Dynamic that is true if the first supplied Dynamic has occurred since the supplied Event has occurred, otherwise false.
updatedSince
  :: forall t m a b.
     ( R.Reflex t
     , R.MonadHold t m
     , MonadFix m
     )
  => R.Dynamic t a
  -> R.Event t b
  -> m (R.Dynamic t Bool)
updatedSince d e = R.zipDynWith
  (\(x :: Integer) (y :: Integer) -> x >= y)
  <$> R.count (R.updated d) <*> R.count e

notUpdatedSince
  :: forall t m a b.
     ( R.Reflex t
     , R.MonadHold t m
     , MonadFix m
     )
  => R.Dynamic t a
  -> R.Event t b
  -> m (R.Dynamic t Bool)
notUpdatedSince d e = fmap not <$> updatedSince d e

createObjectURL :: JS.MonadJSM m => BS.ByteString -> m Text
createObjectURL bs = do
  let opt :: Maybe GHCJS.DOM.Types.BlobPropertyBag = Nothing
  ba <- JSUtils.bsToArrayBuffer bs
  b <- GHCJS.DOM.Blob.newBlob [ba] opt
  url :: JS.JSString <- GHCJS.DOM.URL.createObjectURL b
  return . T.pack . JS.fromJSString $ url
