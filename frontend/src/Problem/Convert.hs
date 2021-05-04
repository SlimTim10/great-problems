{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Problem.Convert
  ( widget
  ) where

import qualified Control.Lens as Lens
import qualified Data.Text as T
import qualified Data.Map as Map

import qualified JSDOM.Types
import qualified JSDOM.FileReader as FileReader
import qualified JSDOM.EventM as EventM
import qualified Language.Javascript.JSaddle as JS
import qualified Reflex.Dom.Core as FRP

import qualified Problem.Types as Types
import Global
import Util

widget
  :: forall t m.
     ( FRP.DomBuilder t m
     , FRP.MonadHold t m
     , JS.MonadJSM (FRP.Performable m)
     , FRP.HasJSContext (FRP.Performable m)
     , FRP.PerformEvent t m
     , FRP.TriggerEvent t m
     , JS.MonadJSM m
     )
  => Types.Options t
  -> FRP.Dynamic t Text
  -> FRP.Dynamic t Text
  -> m (FRP.Dynamic t (Maybe Types.ConvertResponse))
widget options prbName editorContent = FRP.el "div" $ do
  evConvert :: FRP.Event t () <- FRP.button "Convert"

  let evFilesAndConvert = FRP.attach (FRP.current $ Types.files options) evConvert
  FRP.widgetHold_ FRP.blank . FRP.ffor evFilesAndConvert $ \(fs, _) -> do
    let fs' :: [JSDOM.Types.File] = map Types.file fs
    mapM_ printFileContents fs'
  
  let evFormData :: FRP.Event t [Map Text (FRP.FormValue JSDOM.Types.File)] = FRP.pushAlways (const buildFormData) evConvert
  responses :: FRP.Event t [FRP.XhrResponse] <- FRP.postForms "https://icewire.ca/uploadprb" evFormData
  let results :: FRP.Event t [Maybe Text] = map (Lens.view FRP.xhrResponse_responseText) <$> responses
  FRP.el "div" $ do
    result :: FRP.Dynamic t Text <- FRP.holdDyn "" $ T.concat . map (maybe "" id) <$> results
    return $ FRP.decodeText <$> result
  where
    buildFormData :: FRP.PushM t [Map Text (FRP.FormValue JSDOM.Types.File)]
    buildFormData = do
      r <- FRP.sample . FRP.current $ Types.random options
      o <- FRP.sample . FRP.current $ Types.output options
      fs <- FRP.sample . FRP.current $ Types.files options
      nm <- FRP.sample . FRP.current $ prbName
      t <- FRP.sample . FRP.current $ editorContent
      let
        formDataText :: Map Text (FRP.FormValue JSDOM.Types.File) = (
          "prbText" =: FRP.FormValue_Text t
          <> "prbName" =: FRP.FormValue_Text nm
          <> "random" =: FRP.FormValue_Text (if r then "true" else "false")
          <> "outFlag" =: FRP.FormValue_Text o
          <> "submit1" =: FRP.FormValue_Text "putDatabase" -- temporary
          )
        formDataFiles :: Map Text (FRP.FormValue JSDOM.Types.File) = Map.fromList $ flip map fs $ \f ->
          let
            fn = Types.name f
            fval = FRP.FormValue_File (Types.file f) (Just fn)
          in (fn, fval)
      let formData = Map.unions [formDataText, formDataFiles]
      return [formData]

    printFileContents :: JSDOM.Types.File -> m ()
    printFileContents f = do
      fileReader <- JS.liftJSM FileReader.newFileReader
      FileReader.readAsText fileReader (Just f) (Just "utf8" :: Maybe Text)
      e :: FRP.Event t (Maybe Text) <- FRP.wrapDomEvent fileReader (`EventM.on` FileReader.load) . JS.liftJSM $ do
        consoleLog ("fileReader onload" :: Text)
        v <- FileReader.getResult fileReader
        (JS.fromJSVal <=< JS.toJSVal) v
      evFileText :: FRP.Event t Text <- return (FRP.fmapMaybe id e)
      FRP.widgetHold_ FRP.blank . FRP.ffor evFileText $ \fileText -> do
        consoleLog ("fileText:" :: Text)
        consoleLog fileText
