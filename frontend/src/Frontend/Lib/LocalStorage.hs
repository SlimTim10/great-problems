module Frontend.Lib.LocalStorage where

import Common.Lib.Prelude

import qualified Language.Javascript.JSaddle as JS

import qualified Frontend.Lib.Storage as Storage

setItem
  :: JS.MonadJSM m
  => Text -- ^ Key
  -> Text -- ^ Value
  -> m ()
setItem k v = Storage.setItem k v =<< getStorage

getItem
  :: JS.MonadJSM m
  => Text -- ^ Key
  -> m (Maybe Text)
getItem k = Storage.getItem k =<< getStorage

removeItem
  :: JS.MonadJSM m
  => Text -- ^ Key
  -> m ()
removeItem k = Storage.removeItem k =<< getStorage

clear
  :: JS.MonadJSM m
  => m ()
clear = Storage.clear =<< getStorage

getStorage
  :: JS.MonadJSM m
  => m Storage.Storage
getStorage = Storage.getStorage Storage.LocalStorage
