{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Frontend.Lib.Util where

import qualified Control.Monad.Trans.Maybe as MaybeT
import qualified Text.RawString.QQ as QQ
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Control.Lens as Lens
import qualified Data.Aeson as JSON
import qualified Control.Monad.IO.Class as IO
import qualified Web.Cookie as Cookie
import qualified Language.Javascript.JSaddle as JS
import Language.Javascript.JSaddle ((!))
import qualified "ghcjs-dom" GHCJS.DOM.Document as DOM
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.Blob as DOM
import qualified GHCJS.DOM.URL as DOM
import qualified GHCJS.DOM.NodeList as DOM
import qualified GHCJS.DOM.NonElementParentNode as DOM
import qualified GHCJS.DOM.Element as DOM
import qualified GHCJS.DOM as DOM
import qualified Foreign.JavaScript.Utils as JSUtils
import qualified Obelisk.Route as Ob
import qualified Reflex.Dom.Core as R
-- Import patch
import qualified MyReflex.Dom.Xhr.FormData as R'

import Common.Lib.Prelude
import qualified Common.Api.User as User
import qualified Common.Route as Route
import qualified Problem.FormFile as FormFile

default (Text)

consoleLog :: (JS.MonadJSM m, JS.ToJSVal v) => v -> m ()
consoleLog x = void $ JS.liftJSM $ do
  w <- JS.jsg "console"
  w ^. JS.js1 "log" x

-- | Generate a random 32-bit integer in JavaScript
random32 :: JS.MonadJSM m => m Integer
random32 = JS.liftJSM $ do
  x :: JS.JSVal <- JS.eval "Math.floor(Math.random() * (2**32 - 1))"
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

formFile :: FormFile.FormFile -> R'.FormValue DOM.File
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
  -> R.Event t (Map Text (R'.FormValue DOM.File))
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
  let opt :: Maybe DOM.BlobPropertyBag = Nothing
  ba <- JSUtils.bsToArrayBuffer bs
  b <- DOM.newBlob [ba] opt
  url :: JS.JSString <- DOM.createObjectURL b
  return . T.pack . JS.fromJSString $ url

-- See: https://developer.mozilla.org/en-US/docs/Web/API/WindowEventHandlers/onbeforeunload
preventLeaving :: JS.MonadJSM m => Bool -> m ()
preventLeaving b = void $ JS.liftJSM $ do
  case b of
    True -> do
      -- Make listener global so it can be removed
      void $ JS.eval [QQ.r|
window.unloadListener = function(e) {
  e.preventDefault();
  e.returnValue = '';
}
|]
      void $ JS.eval "window.addEventListener('beforeunload', window.unloadListener)"
    False -> void $ JS.eval "window.removeEventListener('beforeunload', window.unloadListener)"

-- | Native browser prompt dialog
-- See: https://developer.mozilla.org/en-US/docs/Web/API/Window/prompt
prompt
  :: JS.MonadJSM m
  => Text -- ^ A message to display in the prompt
  -> m Text
prompt msg = JS.liftJSM $ do
  w <- JS.jsg "window"
  x :: JS.JSVal <- w ^. JS.js1 "prompt" msg
  return . JS.fromJSString . fromMaybe (JS.textToStr "") =<< JS.fromJSVal x

-- | history.back()
-- See: https://developer.mozilla.org/en-US/docs/Web/API/History/back
historyBack :: JS.MonadJSM m => m ()
historyBack = void $ JS.liftJSM $ do
  history <- JS.jsg "history"
  history ^. JS.js0 "back"

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
    window <- JS.jsg "window"
    void $ window ! "location" ^. JS.js1 "replace" url'

placeRawHTML
  :: ( R.DomBuilder t m
     , JS.MonadJSM m
     , JS.ToJSVal (R.RawElement (R.DomBuilderSpace m))
     )
  => Text -- ^ ID attribute for <div> element
  -> Text -- ^ HTML content
  -> m (R.RawElement (R.DomBuilderSpace m))
placeRawHTML divId html = do
  el <- R._element_raw . fst <$> R.elAttr' "div" ("id" =: divId) R.blank
  setInnerHTML el html
  return el

setInnerHTML
  :: ( JS.MonadJSM m
     , JS.ToJSVal (R.RawElement (R.DomBuilderSpace m))
     )
  => R.RawElement (R.DomBuilderSpace m)
  -> Text
  -> m ()
setInnerHTML el html = JS.liftJSM $ void $ MaybeT.runMaybeT $ do
  el' :: DOM.Element <- MaybeT.MaybeT $ (JS.toJSVal >=> JS.fromJSVal) el
  DOM.setInnerHTML el' html

appendScriptURL
  :: ( JS.MonadJSM m
     , JS.ToJSVal (R.RawElement (R.DomBuilderSpace m))
     )
  => R.RawElement (R.DomBuilderSpace m) -- ^ Append script to this element
  -> Text -- ^ Script type (e.g., "text/javascript")
  -> Text -- ^ URL to script
  -> m ()
appendScriptURL el scriptType url = JS.liftJSM $ do
  doc <- JS.jsg "document"
  -- script = document.createElement('script')
  script <- doc ^. JS.js1 "createElement" "script"
  -- script.type = scriptType
  script ^. JS.jss "type" scriptType
  -- script.src = url
  script ^. JS.jss "src" url
  elVal <- JS.toJSVal el
  -- el.appendChild(script)
  void $ (JS.Object elVal) ^. JS.js1 "appendChild" script

appendScript
  :: ( JS.MonadJSM m
     , JS.ToJSVal (R.RawElement (R.DomBuilderSpace m))
     )
  => R.RawElement (R.DomBuilderSpace m) -- ^ Append script to this element
  -> Text -- ^ Script type (e.g., "text/javascript")
  -> Text -- ^ Script text
  -> m ()
appendScript el scriptType txt = JS.liftJSM $ do
  doc <- JS.jsg "document"
  -- script = document.createElement('script')
  script <- doc ^. JS.js1 "createElement" "script"
  -- script.type = scriptType
  script ^. JS.jss "type" scriptType
  -- script.innerHTML = txt
  script ^. JS.jss "innerHTML" txt
  elVal <- JS.toJSVal el
  -- el.appendChild(script)
  void $ (JS.Object elVal) ^. JS.js1 "appendChild" script

nodeListNodes
  :: ( DOM.IsNodeList l
     , JS.MonadJSM m
     )
  =>
  l ->
  m [DOM.Node]
nodeListNodes es = do
  len <- DOM.getLength es
  -- Warning! len is unsigned. If the NodeList is empty, we must avoid
  -- accidentally traversing over [0..maxBound::Word]
  nodes <- traverse (DOM.item es) $
    if len == 0 then [] else [0..len-1]
  pure $ catMaybes nodes

hideElement
  :: JS.MonadJSM m
  => Text -- ^ Element ID
  -> Bool -- ^ True to hide, false to show
  -> m ()
hideElement elId b = JS.liftJSM $ void $ MaybeT.runMaybeT $ do
  doc <- JS.liftJSM $ DOM.currentDocumentUnchecked
  answerElem <- MaybeT.MaybeT $ DOM.getElementById doc elId
  JS.liftJSM $ answerElem ^. JS.jss "hidden" b
        
