{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module ProblemWidget.Convert
  ( convertWidget
  ) where

import Control.Monad
import Control.Lens (view)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map

import JSDOM.Types (File)
import JSDOM.FileReader (newFileReader, readAsText, getResult, load)
import JSDOM.EventM (on)
import Language.Javascript.JSaddle
  ( MonadJSM
  , liftJSM
  , fromJSVal
  , toJSVal
  )

import Reflex.Dom.Core

import Util
import ProblemWidget.Types

convertWidget
  :: forall t m.
     ( DomBuilder t m
     , MonadHold t m
     , PostBuild t m
     , MonadJSM (Performable m)
     , HasJSContext (Performable m)
     , PerformEvent t m
     , TriggerEvent t m
     , MonadJSM m
     )
  => Options t
  -> Dynamic t Text
  -> Dynamic t Text
  -> m ()
convertWidget options prbName editorContent = el "div" $ do
  evConvert :: Event t () <- button "Convert"

  let evFilesAndConvert = attach (current $ files options) evConvert
  widgetHold_ blank . ffor evFilesAndConvert $ \(fs, _) -> do
    let fs' :: [File] = map file fs
    mapM_ printFileContents fs'
  
  let evFormData :: Event t [Map Text (FormValue File)] = pushAlways (const buildFormData) evConvert
  responses :: Event t [XhrResponse] <- postForms "/uploadprb" evFormData
  let results = map (view xhrResponse_responseText) <$> responses
  el "div" $ do
    asText <- holdDyn "No results." $ T.pack . concat . map (maybe "" show) <$> results
    dynText asText
  where
    buildFormData :: PushM t [Map Text (FormValue File)]
    buildFormData = do
      r <- sample . current $ random options
      o <- sample . current $ output options
      fs <- sample . current $ files options
      nm <- sample . current $ prbName
      t <- sample . current $ editorContent
      let
        formDataText :: Map Text (FormValue File) = (
          "prbText" =: FormValue_Text t <>
          "prbName" =: FormValue_Text nm <>
          "random" =: FormValue_Text (showText r) <>
          "outFlag" =: FormValue_Text o <>
          "submit1" =: FormValue_Text "putDatabase" -- temporary
          )
        formDataFiles :: Map Text (FormValue File) = Map.fromList $ flip map fs $ \f ->
          let
            fn = name f
            fval = FormValue_File (file f) (Just fn)
          in (fn, fval)
      let formData = Map.unions [formDataText, formDataFiles]
      return [formData]

    printFileContents :: File -> m ()
    printFileContents f = do
      fileReader <- liftJSM newFileReader
      readAsText fileReader (Just f) (Just "utf8" :: Maybe Text)
      e :: Event t (Maybe Text) <- wrapDomEvent fileReader (`on` load) . liftJSM $ do
        consoleLog ("fileReader onload" :: Text)
        v <- getResult fileReader
        (fromJSVal <=< toJSVal) v
      evFileText :: Event t Text <- return (fmapMaybe id e)
      widgetHold_ blank . ffor evFileText $ \fileText -> do
        consoleLog ("fileText:" :: Text)
        consoleLog fileText
