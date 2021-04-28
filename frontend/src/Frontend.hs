{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Frontend where

import Control.Monad
import Control.Monad.Fix
import qualified Data.Text as T
-- import qualified Data.Text.Encoding as T
import qualified Data.Map as Map
import Language.Javascript.JSaddle
  ( MonadJSM
  -- , eval
  -- , liftJSM
  )
import JSDOM.Types (File)
import JSDOM.File (getName)
import Data.Maybe (fromJust)
import Data.List (nub)

import Obelisk.Frontend
-- import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static

import Reflex.Dom.Core

-- import Common.Api
import Common.Route


frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Great Problems"
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do
      el "h1" $ text "Problem to Tex"

      prerender_ blank $ options
  }

options
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     , MonadJSM m
     )
  => m ()
options = do
  borderBox $ do
    elClass "div" "mainContainer" $ do
      elClass "div" "optionsContainer" $ do
        el "h2" $ text "Options"
        randomOption
        outputOption
        drawingsOption

drawingsOption
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     , MonadJSM m
     )
  => m ()
drawingsOption = el "div" $ do
  el "h4" $ text "Drawings"
  fi <- el "label" $ do
    text "Upload"
    fi1 <- inputElement $ def & initialAttributes .~ (
      "type" =: "file" <>
      "accept" =: ".asc" <>
      "multiple" =: ""
      )
    return fi1
  drawingsWidget $ _inputElement_files fi
  return ()

drawingsWidget
  :: forall t m.
    ( DomBuilder t m
     , MonadHold t m
     , PostBuild t m
     , MonadFix m
     , MonadJSM m
     )
  => Dynamic t [File]
  -> m ()
drawingsWidget drawingsState = do
  dFiles :: Dynamic t [File] <- accumDyn (<>) [] (updated drawingsState)
  filesState :: Dynamic t [FileWithName] <- simpleList dFiles mkFile
  uniqueFilesState :: Dynamic t [FileWithName] <- accumDyn collectFiles [] (updated filesState)

  el "ul" $ do
    void $ simpleList uniqueFilesState fileItem
    return ()

  where
    collectFiles :: [FileWithName] -> [FileWithName] -> [FileWithName]
    collectFiles state newFiles = nub $ state <> newFiles

    fileItem :: Dynamic t FileWithName -> m ()
    fileItem dFile = el "li" $ do
      dynText $ name <$> dFile

    mkFile :: Dynamic t File -> m FileWithName
    mkFile dFile = do
      let dName :: Dynamic t (m T.Text) = getName <$> dFile
      mName :: m T.Text <- sample . current $ dName
      f :: File <- sample . current $ dFile
      n :: T.Text <- mName
      return $ FileWithName f n

data FileWithName = FileWithName
  { file :: File
  , name :: T.Text
  }

instance Show FileWithName where
  show = show . name

instance Eq FileWithName where
  (==) a b = name a == name b

randomOption :: (DomBuilder t m, PostBuild t m) => m ()
randomOption = el "div" $ do
  cb <- el "label" $ do
    cb1 <- inputElement $ def & initialAttributes .~ (
      "type" =: "checkbox"
      )
    text "Randomize variables"
    return cb1
  let dynState = randomState <$> _inputElement_checked cb
  el "p" $ do
    dynText dynState
  return ()
  where
    randomState True = "Randomize variables is checked"
    randomState _ = "Randomize variables is not checked"

outputOption
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     )
  => m ()
outputOption = el "div" $ do
  dd <- el "label" $ do
    text "Output"
    dd1 <- dropdown "flagSolutions" (constDyn items) def
    return dd1
  let selItem = result <$> value dd
  el "p" $ do
    dynText selItem
  return ()
  where
    items :: Map.Map T.Text T.Text
    items = Map.fromList
      [ ("flagSolutions", "with solutions")
      , ("flagAnswers", "with answer")
      , ("flagSolAns", "with solution and answer")
      , ("flagQuestions", "questions only")
      ]
    result key = "You selected: " <> fromJust (Map.lookup key items)

elLabel :: DomBuilder t m => T.Text -> T.Text -> m ()
elLabel for label = elAttr "label" ("for" =: for) $ text label

borderBox :: DomBuilder t m => m () -> m ()
borderBox = elAttr "div" ("style" =: "border: 1px solid black;")


{-
formData.append('prbText', getEditorContent(editorRef))
formData.append('prbName', documentName)
formData.append('random', randomFlag)
formData.append('outFlag', outputType)
formData.append('submit1', 'putDatabase') // temporary
-}
