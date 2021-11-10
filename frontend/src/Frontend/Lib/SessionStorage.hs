module Frontend.Lib.SessionStorage where

import Common.Lib.Prelude

import qualified Language.Javascript.JSaddle as JS
import qualified GHCJS.DOM
import qualified GHCJS.DOM.Window
import qualified GHCJS.DOM.Storage

setItem
  :: JS.MonadJSM m
  => Text -- ^ Key
  -> Text -- ^ Value
  -> m ()
setItem k v = void $ JS.liftJSM $ do
  window <- GHCJS.DOM.currentWindowUnchecked
  storage <- GHCJS.DOM.Window.getSessionStorage window
  GHCJS.DOM.Storage.setItem storage k v

getItem
  :: JS.MonadJSM m
  => Text -- ^ Key
  -> m (Maybe Text)
getItem k = JS.liftJSM $ do
  window <- GHCJS.DOM.currentWindowUnchecked
  storage <- GHCJS.DOM.Window.getSessionStorage window
  GHCJS.DOM.Storage.getItem storage k

removeItem
  :: JS.MonadJSM m
  => Text -- ^ Key
  -> m ()
removeItem k = void $ JS.liftJSM $ do
  window <- GHCJS.DOM.currentWindowUnchecked
  storage <- GHCJS.DOM.Window.getSessionStorage window
  GHCJS.DOM.Storage.removeItem storage k

clear
  :: JS.MonadJSM m
  => m ()
clear = void $ JS.liftJSM $ do
  window <- GHCJS.DOM.currentWindowUnchecked
  storage <- GHCJS.DOM.Window.getSessionStorage window
  GHCJS.DOM.Storage.clear storage
