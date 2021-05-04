{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Problem.Options
  ( widget
  ) where

import qualified Control.Monad.Fix as Fix
import qualified Data.List as List
import qualified Data.Map as Map

import qualified JSDOM.File
import qualified JSDOM.Types
import qualified Language.Javascript.JSaddle as JS
import qualified Reflex.Dom.Core as FRP

import qualified Problem.Types as Types
import Global
import Util

widget
  :: ( FRP.DomBuilder t m
     , FRP.PostBuild t m
     , FRP.MonadHold t m
     , Fix.MonadFix m
     , JS.MonadJSM m
     )
  => m (Types.Options t)
widget = do
  FRP.elAttr "div" ("style" =: "border: 1px solid black;") $ do
    FRP.elClass "div" "mainContainer" $ do
      FRP.elClass "div" "optionsContainer" $ do
        FRP.el "h2" $ FRP.text "Options"
        r :: FRP.Dynamic t Bool <- randomOption
        o :: FRP.Dynamic t Text <- outputOption
        fs :: FRP.Dynamic t [Types.FileWithName] <- drawingsOption
        return $ Types.Options r o fs

drawingsOption
  :: ( FRP.DomBuilder t m
     , FRP.PostBuild t m
     , FRP.MonadHold t m
     , Fix.MonadFix m
     , JS.MonadJSM m
     )
  => m (FRP.Dynamic t [Types.FileWithName])
drawingsOption = FRP.el "div" $ do
  FRP.el "h4" $ FRP.text "Drawings"
  fi <- FRP.el "label" $ do
    FRP.text "Upload"
    fi1 <- FRP.inputElement $ FRP.def & FRP.initialAttributes .~ (
      "type" =: "file"
      <> "accept" =: ".asc"
      <> "multiple" =: ""
      )
    return fi1
  fs <- drawingsWidget $ FRP._inputElement_files fi
  return fs

drawingsWidget
  :: forall t m.
     ( FRP.DomBuilder t m
     , FRP.MonadHold t m
     , FRP.PostBuild t m
     , Fix.MonadFix m
     , JS.MonadJSM m
     )
  => FRP.Dynamic t [JSDOM.Types.File]
  -> m (FRP.Dynamic t [Types.FileWithName])
drawingsWidget drawingsState = do
  dFiles :: FRP.Dynamic t [JSDOM.Types.File] <- FRP.accumDyn (<>) [] (FRP.updated drawingsState)
  filesState :: FRP.Dynamic t [Types.FileWithName] <- FRP.simpleList dFiles mkFile
  uniqueFilesState :: FRP.Dynamic t [Types.FileWithName] <- FRP.accumDyn collectFiles [] (FRP.updated filesState)
  FRP.el "ul" $ do
    void $ FRP.simpleList uniqueFilesState $ \dFile -> do
      FRP.el "li" $ do
        FRP.dynText $ Types.name <$> dFile
  return uniqueFilesState

  where
    collectFiles :: [Types.FileWithName] -> [Types.FileWithName] -> [Types.FileWithName]
    collectFiles state newFiles = List.nub $ state <> newFiles

    mkFile :: FRP.Dynamic t JSDOM.Types.File -> m Types.FileWithName
    mkFile dFile = do
      let dName :: FRP.Dynamic t (m Text) = JSDOM.File.getName <$> dFile
      mName :: m Text <- FRP.sample . FRP.current $ dName
      f :: JSDOM.Types.File <- FRP.sample . FRP.current $ dFile
      n :: Text <- mName
      consoleLog ("mkFile" :: Text)
      consoleLog f
      return $ Types.FileWithName f n

randomOption :: (FRP.DomBuilder t m) => m (FRP.Dynamic t Bool)
randomOption = FRP.el "div" $ do
  cb <- FRP.el "label" $ do
    cb1 <- FRP.inputElement $ FRP.def & FRP.initialAttributes .~ (
      "type" =: "checkbox"
      )
    FRP.text "Randomize variables"
    return cb1
  return $ FRP._inputElement_checked cb

outputOption
  :: ( FRP.DomBuilder t m
     , FRP.PostBuild t m
     , FRP.MonadHold t m
     , Fix.MonadFix m
     )
  => m (FRP.Dynamic t Text)
outputOption = FRP.el "div" $ do
  dd <- FRP.el "label" $ do
    FRP.text "Output"
    dd1 <- FRP.dropdown "flagSolutions" (FRP.constDyn items) FRP.def
    return dd1
  return $ FRP.value dd
  where
    items :: Map Text Text
    items = Map.fromList
      [ ("flagSolutions", "with solutions")
      , ("flagAnswers", "with answer")
      , ("flagSolAns", "with solution and answer")
      , ("flagQuestions", "questions only")
      ]
