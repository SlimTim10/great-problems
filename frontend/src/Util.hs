module Util where

import qualified Language.Javascript.JSaddle as JS
import qualified Data.Text as T

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
