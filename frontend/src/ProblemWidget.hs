{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module ProblemWidget
  ( problemWidget
  ) where

import Control.Monad.Fix

import Language.Javascript.JSaddle (MonadJSM)

import Reflex.Dom.Core

import ProblemWidget.Types
import ProblemWidget.Options
import ProblemWidget.Convert

problemWidget
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     , MonadJSM m
     , MonadJSM (Performable m)
     , HasJSContext (Performable m)
     , PerformEvent t m
     , TriggerEvent t m
     )
  => m ()
problemWidget = do
  options :: Options t <- optionsWidget
  convertWidget options
