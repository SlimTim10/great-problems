{-# LANGUAGE PackageImports #-}
module Util where

import qualified Language.Javascript.JSaddle as JS
import qualified Data.Text as T
import qualified Data.Aeson as JSON
import qualified Control.Monad.IO.Class as IO
import qualified Crypto.PasswordStore
import qualified Reflex.Dom.Core as R
import qualified "jsaddle-dom" GHCJS.DOM.Document as DOM
import qualified Web.Cookie as Cookie

import qualified Common.Api.User as User
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
