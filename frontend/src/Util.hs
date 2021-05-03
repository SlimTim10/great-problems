{-# LANGUAGE OverloadedStrings #-}

module Util where

import Control.Lens ((^.))
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import Language.Javascript.JSaddle
  ( MonadJSM
  , liftJSM
  , jsg
  , js1
  , ToJSVal
  )

showText :: Show s => s -> Text
showText = T.pack . show

consoleLog :: (MonadJSM m, ToJSVal v) => v -> m ()
consoleLog x = void $ liftJSM $ do
  w <- jsg ("console" :: Text)
  w ^. js1 ("log" :: Text) x
