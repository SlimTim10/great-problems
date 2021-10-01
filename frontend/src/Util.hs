{-# LANGUAGE PackageImports #-}
module Util where

import qualified Language.Javascript.JSaddle as JS
import qualified Data.Text as T
import qualified Control.Lens as Lens
import qualified Data.Aeson as JSON
import qualified Text.Read
import qualified Control.Monad.IO.Class as IO
import qualified Crypto.PasswordStore
import qualified "jsaddle-dom" GHCJS.DOM.Document as DOM
import qualified JSDOM.Types
import qualified Web.Cookie as Cookie
import qualified System.Environment as Env
import qualified Reflex.Dom.Core as R
-- Import patch
import qualified MyReflex.Dom.Xhr.FormData as R'

import qualified Common.Api.User as User
import qualified Common.File as File
import Global

showText :: Show s => s -> Text
showText = T.pack . show

consoleLog :: (JS.MonadJSM m, JS.ToJSVal v) => v -> m ()
consoleLog x = void $ JS.liftJSM $ do
  w <- JS.jsg ("console" :: Text)
  w ^. JS.js1 ("log" :: Text) x

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:_) = Just x

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

whenM :: Monad m => Bool -> (a -> m a) -> a -> m a
whenM b f m = (if b then f else return) m

hashPassword :: Text -> IO Text
hashPassword password = cs <$> Crypto.PasswordStore.makePassword (cs $ password) 17

verifyPassword :: Text -> Text -> Bool
verifyPassword a b = Crypto.PasswordStore.verifyPassword (cs a) (cs b)

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

-- | Look up an environment variable, given a default to fall back to.
lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
  maybeEnv <- Env.lookupEnv env
  case maybeEnv of
    Nothing -> pure def
    Just str -> maybe (pure . read . show $ str) pure (Text.Read.readMaybe str)

formFile :: File.FileWithName -> R'.FormValue JSDOM.Types.File
formFile f = R'.FormValue_File (File.file f) (Just (File.name f))

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
  -> R.Event t (Map Text (R'.FormValue JSDOM.Types.File))
  -> m (R.Event t Text)
postForm url formData = do
  formDatas :: R.Event t [Map Text (R'.FormValue JSDOM.Types.File)] <- R.performEvent
    $ R.ffor formData $ \fd -> return [fd]
  responses <- R'.postForms url formDatas
  let results :: R.Event t [Maybe Text] = map (Lens.view R.xhrResponse_responseText) <$> responses
  return $ T.concat . map (maybe "" id) <$> results
