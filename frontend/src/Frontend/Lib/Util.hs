{-# LANGUAGE PackageImports #-}
module Frontend.Lib.Util where

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
import qualified Obelisk.Route as Ob
import qualified Reflex.Dom.Core as R
-- Import patch
import qualified MyReflex.Dom.Xhr.FormData as R'

import Common.Lib.Prelude
import qualified Common.Api.User as User
import qualified Common.Route as Route
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

-- | Make posting forms less awkward. Send a single map of params and return the response as text.
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
  return $ T.concat . map (fromMaybe "") <$> results

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

-- See: https://developer.mozilla.org/en-US/docs/Web/API/WindowEventHandlers/onbeforeunload
preventLeaving :: JS.MonadJSM m => Bool -> m ()
preventLeaving b = void $ JS.liftJSM $ do
  case b of
    True -> do
      -- Make listener global so it can be removed
      void $ JS.eval (
        "\
        \ window.unloadListener = function(e) { \
        \   e.preventDefault(); \
        \   e.returnValue = ''; \
        \ } \
        \ " :: Text)
      void $ JS.eval ("window.addEventListener('beforeunload', window.unloadListener)" :: Text)
    False -> void $ JS.eval ("window.removeEventListener('beforeunload', window.unloadListener)" :: Text)

-- | Native browser prompt dialog
-- See: https://developer.mozilla.org/en-US/docs/Web/API/Window/prompt
prompt
  :: JS.MonadJSM m
  => Text -- ^ A message to display in the prompt
  -> m Text
prompt msg = JS.liftJSM $ do
  w <- JS.jsg ("window" :: Text)
  x :: JS.JSVal <- w ^. JS.js1 ("prompt" :: Text) msg
  return . JS.fromJSString . fromMaybe (JS.textToStr "") =<< JS.fromJSVal x

-- | history.back()
-- See: https://developer.mozilla.org/en-US/docs/Web/API/History/back
historyBack :: JS.MonadJSM m => m ()
historyBack = void $ JS.liftJSM $ do
  history <- JS.jsg ("history" :: Text)
  history ^. JS.js0 ("back" :: Text)

-- | window.location.replace(url)
redirectWithoutHistory
  :: forall t m.
     ( JS.MonadJSM (R.Performable m)
     , R.PerformEvent t m
     )
  => R.Event t (Ob.R Route.FrontendRoute)
  -> m ()
redirectWithoutHistory url = do
  let textUrl :: R.Event t Text = Route.frontendHref <$> url
  R.performEvent_ $ R.ffor textUrl $ \url' -> void $ JS.liftJSM $ do
    window <- JS.jsg ("window" :: Text)
    void $ window ^. JS.js ("location" :: Text) ^. JS.js1 ("replace" :: Text) url'

placeRawHTML
  :: ( R.DomBuilder t m
     , JS.MonadJSM m
     , JS.ToJSVal (R.RawElement (R.DomBuilderSpace m))
     )
  => Text
  -> m (R.RawElement (R.DomBuilderSpace m))
placeRawHTML html = do
  el <- R._element_raw . fst <$> R.el' "div" R.blank
  setInnerHTML el html
  return el

setInnerHTML
  :: ( JS.MonadJSM m
     , JS.ToJSVal (R.RawElement (R.DomBuilderSpace m))
     )
  => R.RawElement (R.DomBuilderSpace m)
  -> Text
  -> m ()
setInnerHTML el html = JS.liftJSM $ do
  htmlVal <- JS.toJSVal html
  elVal <- JS.toJSVal el
  JS.setProp "innerHTML" htmlVal (JS.Object elVal)

appendScript
  :: ( JS.MonadJSM m
     , JS.ToJSVal (R.RawElement (R.DomBuilderSpace m))
     )
  => R.RawElement (R.DomBuilderSpace m)
  -> Text
  -> m ()
appendScript el scriptUrl = JS.liftJSM $ do
  doc <- JS.jsg ("document" :: Text)
  -- script = document.createElement('script')
  script <- doc ^. JS.js1 ("createElement" :: Text) ("script" :: Text)
  -- script.src = scriptUrl
  void $ script ^. JS.jss ("src" :: Text) scriptUrl
  elVal <- JS.toJSVal el
  -- el.appendChild(script)
  void $ (JS.Object elVal) ^. JS.js1 ("appendChild" :: Text) script
