{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}

module Frontend where

import Control.Monad
import Control.Monad.Fix
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Map as Map
import Language.Javascript.JSaddle (eval, liftJSM, valToJSON)
import JSDOM.Types (unFile, fromJSVal, JSVal(..), File)
import Data.Maybe (fromJust)
import qualified GHCJS.DOM.HTMLInputElement as Input

import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static

import Reflex.Dom.Core

import Common.Api
import Common.Route


frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Great Problems"
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do
      el "h1" $ text "Problem to Tex"

      options
  }

options :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => m ()
options = do
  borderBox $ do
    elClass "div" "mainContainer" $ do
      elClass "div" "optionsContainer" $ do
        el "h2" $ text "Options"
        randomOption
        outputOption
        drawingsOption
        
        -- el "div" $ do
        --   el "h4" $ text "Drawings"
        --   elLabel idUploadDrawing "Upload"
        --   elAttr "input" (
        --     "name" =: nameUploadDrawing <>
        --     "id" =: idUploadDrawing <>
        --     "type" =: "file" <>
        --     "accept" =: ".asc" <>
        --     "multiple" =: "multiple"
        --     ) blank
  where
    idRandom = "random"
    nameRandom = "random"
    idOutput = "selectOutput"
    nameOutput = "outFlag"
    idUploadDrawing = "uploadDrawing"
    nameUploadDrawing = "uploadDrawing"

drawingsOption :: (DomBuilder t m, PostBuild t m) => m ()
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
  let dynState = drawingsState <$> _inputElement_files fi
  -- let dynState = T.pack . show . map ((show :: JSVal -> String) . unFile) <$> _inputElement_files fi
  dynText dynState
  return ()

drawingsState :: [File] -> T.Text
drawingsState fs = T.pack . show $ length fs

randomOption :: (DomBuilder t m, PostBuild t m) => m ()
randomOption = el "div" $ do
  cb <- el "label" $ do
    cb1 <- checkbox False def
    text "Randomize variables"
    return cb1
  -- let dynState = randomState <$> value cb
  -- dynText dynState
  return ()
  where
    randomState True = "Randomize variables is checked"
    randomState _ = "Randomize variables is not checked"

outputOption :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => m ()
outputOption = el "div" $ do
  dd <- el "label" $ do
    text "Output"
    dd1 <- dropdown "flagSolutions" (constDyn items) def
    return dd1
  -- let selItem = result <$> value dd
  -- dynText selItem
  return ()
  where
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
