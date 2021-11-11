module Frontend.Lib.Storage where

import Common.Lib.Prelude

import qualified Language.Javascript.JSaddle as JS
import qualified GHCJS.DOM
import qualified GHCJS.DOM.Window
import qualified GHCJS.DOM.Storage

data StorageType = LocalStorage | SessionStorage

type Storage = GHCJS.DOM.Storage.Storage

getStorage
  :: JS.MonadJSM m
  => StorageType
  -> m Storage
getStorage st = do
  window <- GHCJS.DOM.currentWindowUnchecked
  case st of
    LocalStorage -> GHCJS.DOM.Window.getLocalStorage window
    SessionStorage -> GHCJS.DOM.Window.getSessionStorage window

setItem
  :: JS.MonadJSM m
  => Text -- ^ Key
  -> Text -- ^ Value
  -> Storage
  -> m ()
setItem k v storage = GHCJS.DOM.Storage.setItem storage k v

getItem
  :: JS.MonadJSM m
  => Text -- ^ Key
  -> Storage
  -> m (Maybe Text)
getItem k storage = GHCJS.DOM.Storage.getItem storage k

removeItem
  :: JS.MonadJSM m
  => Text -- ^ Key
  -> Storage
  -> m ()
removeItem k storage = GHCJS.DOM.Storage.removeItem storage k

clear
  :: JS.MonadJSM m
  => Storage
  -> m ()
clear storage = GHCJS.DOM.Storage.clear storage
