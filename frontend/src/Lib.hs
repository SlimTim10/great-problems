{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
  ( postForm
  , MyFormValue(..)
  ) where

import Control.Lens (iforM_)
import Data.Text (Text)
import Data.Map (Map)
import Language.Javascript.JSaddle (MonadJSM)
import qualified GHCJS.DOM.FormData as FD
import JSDOM.Types (IsBlob)

import Reflex.Dom.Core

data MyFormValue blob
  = MyFormValue_Text Text
  | MyFormValue_File blob (Maybe Text) -- maybe filename

postForm
  :: ( HasJSContext (Performable m)
     , MonadJSM (Performable m)
     , PerformEvent t m
     , TriggerEvent t m
     , IsBlob blob
     )
  => Text -- ^ The target url
  -> Event t (Map Text (MyFormValue blob))
  -> m (Event t XhrResponse)
postForm url payload = do
  performMkRequestAsync $ ffor payload $ \u -> do
    fd <- FD.newFormData Nothing
    iforM_ u $ \k v -> case v of
      MyFormValue_Text t -> FD.append fd k t
      MyFormValue_File b fn -> FD.appendBlob fd k b fn
    return $ xhrRequest "POST" url $ def & xhrRequestConfig_sendData .~ fd
