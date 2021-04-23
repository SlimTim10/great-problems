{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}

module Frontend where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Map as M
import Language.Javascript.JSaddle (eval, liftJSM)

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

options :: DomBuilder t m => m ()
options = do
  borderBox $ do
    elClass "div" "mainContainer" $ do
      elClass "div" "optionsContainer" $ do
        el "h2" $ text "Options"
        el "div" $ do
          elAttr "input" (
            "name" =: nameRandom <>
            "type" =: "checkbox" <>
            "id" =: idRandom
            ) blank
          elAttr "label" ("for" =: idRandom) $ text "Randomize variables"
        el "div" $ do
          elAttr "label" ("for" =: idOutput) $ text "Output"
          elAttr "select" ("name" =: nameOutput <> "id" =: idOutput) $ do
            makeOption "flagSolutions" "with solution"
            makeOption "flagAnswers" "with answer"
            makeOption "flagSolAns" "with solution and answer"
            makeOption "flagQuestions" "questions only"
        el "div" $ do
          el "h4" $ text "Drawings"
  where
    idRandom = "random"
    nameRandom = "random"
    idOutput = "selectOutput"
    nameOutput = "outFlag"

makeOption :: DomBuilder t m => T.Text -> T.Text -> m ()
makeOption val label = elAttr "option" ("value" =: val) $ text label

borderBox :: DomBuilder t m => m () -> m ()
borderBox = elAttr "div" ("style" =: "border: 1px solid black;")


{-
formData.append('prbText', getEditorContent(editorRef))
formData.append('prbName', documentName)
formData.append('random', randomFlag)
formData.append('outFlag', outputType)
formData.append('submit1', 'putDatabase') // temporary
-}
