{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Frontend where

-- import Control.Monad
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
     , Prerender js t m
     )
  => m ()
options = do
  borderBox $ do
    elClass "div" "mainContainer" $ do
      elClass "div" "optionsContainer" $ do
        el "h2" $ text "Options"
        randomOption
        outputOption
        prerender_ blank $ drawingsOption
        
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
  -- let dynState = drawingsState <$> _inputElement_files fi -- :: Dynamic t T.Text
  -- dynText dynState
  drawingsWidget $ _inputElement_files fi
  return ()

drawingsWidget
  :: ( DomBuilder t m
     , MonadHold t m
     , PostBuild t m
     , MonadFix m
     , MonadJSM m
     )
  => Dynamic t [File]
  -> m (Dynamic t [()])
drawingsWidget drawingsDyn = simpleList drawingsDyn drawingWidget

drawingWidget
  :: forall t m.
     ( DomBuilder t m
     , MonadHold t m
     , PostBuild t m
     , MonadFix m
     , MonadJSM m
     )
  => Dynamic t File
  -> m ()
drawingWidget drawingDyn = el "div" $ do
  let dynNameAction :: Dynamic t (m T.Text) = getName <$> drawingDyn
  getNameEvent :: Event t T.Text <- dyn dynNameAction
  name :: Dynamic t T.Text <- holdDyn "" getNameEvent
  dynText name

  let getFileEvent :: Event t File = updated drawingDyn

  el "div" blank
  el "ul" $ do
    dFiles :: Dynamic t [File] <- accumDyn collectFiles [] getFileEvent
    simpleList dFiles fileItem
    
    -- dstate :: Dynamic t [T.Text] <- accumDyn collectNames [initialState] getNameEvent
    -- display dstate
  
  return ()
  where
    initialState :: T.Text
    initialState = T.empty

    collectNames :: [T.Text] -> T.Text -> [T.Text]
    collectNames state newName = state <> [newName]

    collectFiles :: [File] -> File -> [File]
    collectFiles state newFile = state <> [newFile]

    fileItem :: Dynamic t File -> m ()
    fileItem dFile = do
      let dynNameAction :: Dynamic t (m T.Text) = getName <$> dFile
      getNameEvent :: Event t T.Text <- dyn dynNameAction
      name :: Dynamic t T.Text <- holdDyn "" getNameEvent
      el "li" $ do
        dynText name

    -- getNameEvent' :: (MonadJSM m) => File -> m (Event t T.Text)
    -- getNameEvent' f = do
    --   let dynNameAction' = getName <$> constDyn f
    --   x <- dyn dynNameAction'
    --   return x

    -- filesToNames
    --   :: ( DomBuilder t m
    --      , PostBuild t m
    --      , MonadHold t m
    --      , MonadFix m
    --      , MonadJSM m
    --      )
    --   => Dynamic t [File] -> Dynamic t [T.Text]
    -- filesToNames dFiles = do
    --   let dynNameActions :: Dynamic t [m T.Text] = map getName <$> dFiles
    --   y :: Event t [T.Text] <- map _ <$> dynNameActions
    --   -- let y :: Dynamic t [T.Text] = map _ <$> dynNameActions
    --   -- getNameEvents :: _ <- dyn dynNameActions
    --   y
    --   -- let dynNamesAction :: Dynamic t (m [T.Text]) = getName <$> dFiles
    --   -- getNameEvents :: Event t [T.Text] <- dyn dynNamesAction
    --   -- names :: Dynamic t [T.Text] <- holdDyn "" getNameEvents
    --   -- return names

drawingsState :: [File] -> T.Text
drawingsState fs = T.pack . show $ length fs

-- drawingsStates :: [File] -> [T.Text]
-- drawingsStates fs = map (getName) $ fs

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
