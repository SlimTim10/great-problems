{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ProblemWidget.Options
  ( optionsWidget
  ) where

import Control.Monad
import Control.Monad.Fix
import Data.List (nub)
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map

import JSDOM.File (getName)
import JSDOM.Types (File)
import Language.Javascript.JSaddle (MonadJSM)

import Reflex.Dom.Core

import Util
import ProblemWidget.Types

optionsWidget
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     , MonadJSM m
     )
  => m (Options t)
optionsWidget = do
  elAttr "div" ("style" =: "border: 1px solid black;") $ do
    elClass "div" "mainContainer" $ do
      elClass "div" "optionsContainer" $ do
        el "h2" $ text "Options"
        r :: Dynamic t Bool <- randomOption
        o :: Dynamic t Text <- outputOption
        fs :: Dynamic t [FileWithName] <- drawingsOption
        return $ Options r o fs

drawingsOption
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     , MonadJSM m
     )
  => m (Dynamic t [FileWithName])
drawingsOption = el "div" $ do
  el "h4" $ text "Drawings"
  fi <- el "label" $ do
    text "Upload"
    fi1 <- inputElement $ def & initialAttributes .~ (
      "type" =: "file"
      <> "accept" =: ".asc"
      <> "multiple" =: ""
      )
    return fi1
  fs <- drawingsWidget $ _inputElement_files fi
  return fs

drawingsWidget
  :: forall t m.
     ( DomBuilder t m
     , MonadHold t m
     , PostBuild t m
     , MonadFix m
     , MonadJSM m
     )
  => Dynamic t [File]
  -> m (Dynamic t [FileWithName])
drawingsWidget drawingsState = do
  dFiles :: Dynamic t [File] <- accumDyn (<>) [] (updated drawingsState)
  filesState :: Dynamic t [FileWithName] <- simpleList dFiles mkFile
  uniqueFilesState :: Dynamic t [FileWithName] <- accumDyn collectFiles [] (updated filesState)
  el "ul" $ do
    void $ simpleList uniqueFilesState $ \dFile -> do
      el "li" $ do
        dynText $ name <$> dFile
  return uniqueFilesState

  where
    collectFiles :: [FileWithName] -> [FileWithName] -> [FileWithName]
    collectFiles state newFiles = nub $ state <> newFiles

    mkFile :: Dynamic t File -> m FileWithName
    mkFile dFile = do
      let dName :: Dynamic t (m Text) = getName <$> dFile
      mName :: m Text <- sample . current $ dName
      f :: File <- sample . current $ dFile
      n :: Text <- mName
      consoleLog ("mkFile" :: Text)
      consoleLog f
      return $ FileWithName f n

randomOption :: (DomBuilder t m) => m (Dynamic t Bool)
randomOption = el "div" $ do
  cb <- el "label" $ do
    cb1 <- inputElement $ def & initialAttributes .~ (
      "type" =: "checkbox"
      )
    text "Randomize variables"
    return cb1
  return $ _inputElement_checked cb

outputOption
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     )
  => m (Dynamic t Text)
outputOption = el "div" $ do
  dd <- el "label" $ do
    text "Output"
    dd1 <- dropdown "flagSolutions" (constDyn items) def
    return dd1
  return $ value dd
  where
    items :: Map Text Text
    items = Map.fromList
      [ ("flagSolutions", "with solutions")
      , ("flagAnswers", "with answer")
      , ("flagSolAns", "with solution and answer")
      , ("flagQuestions", "questions only")
      ]
