{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Frontend where

import Control.Monad
import Control.Monad.Fix
import Data.Text (Text)
import qualified Data.Text as T
-- import qualified Data.Text.Encoding as T
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, maybe)
import Data.List (nub)
import Control.Lens (view)
import Language.Javascript.JSaddle
  ( MonadJSM
  -- , eval
  -- , liftJSM
  )
import JSDOM.Types (File, IsBlob)
import JSDOM.File (getName)
import qualified JSDOM.Text as JS

import Obelisk.Frontend
-- import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static

import Reflex.Dom.Core

-- import Common.Api
import Common.Route

import Lib


frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Great Problems"
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do
      el "h1" $ text "Problem to Tex"

      prerender_ blank $ optionsWidget
      prerender_ blank $ convertWidget
  }

optionsWidget
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     , MonadJSM m
     )
  => m ()
optionsWidget = do
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
      let dName :: Dynamic t (m Text) = getName <$> dFile
      mName :: m Text <- sample . current $ dName
      f :: File <- sample . current $ dFile
      n :: Text <- mName
      return $ FileWithName f n

data FileWithName = FileWithName
  { file :: File
  , name :: Text
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
    items :: Map Text Text
    items = Map.fromList
      [ ("flagSolutions", "with solutions")
      , ("flagAnswers", "with answer")
      , ("flagSolAns", "with solution and answer")
      , ("flagQuestions", "questions only")
      ]
    result key = "You selected: " <> fromJust (Map.lookup key items)

elLabel :: DomBuilder t m => Text -> Text -> m ()
elLabel for label = elAttr "label" ("for" =: for) $ text label

borderBox :: DomBuilder t m => m () -> m ()
borderBox = elAttr "div" ("style" =: "border: 1px solid black;")

convertWidget
  :: forall t m.
     ( DomBuilder t m
     , MonadHold t m
     , PostBuild t m
     , MonadJSM (Performable m)
     , HasJSContext (Performable m)
     , PerformEvent t m
     , TriggerEvent t m
     )
  => m ()
convertWidget = el "div" $ do
  let x :: FormValue File = FormValue_Text "hellooooooooo"
  let formData :: Map Text (FormValue File) = ("test" =: x <> "test2" =: FormValue_Text "blah")
  let dFormData :: Dynamic t [Map Text (FormValue File)] = constDyn [formData]
  let eFormData = updated dFormData
  responses :: Event t [XhrResponse] <- postForms "/uploadprb" eFormData
  let results = map (view xhrResponse_responseText) <$> responses

  (e1, trigger) <- newTriggerEvent

  asText <- holdDyn "No results." $ T.pack . concat . map (maybe "x" show) <$> results
  dynText asText
  return ()

  let x :: MyFormValue File = MyFormValue_Text "hellooooooooo"
  let formData :: Map Text (MyFormValue File) = ("test" =: x)
  let dFormData :: Dynamic t (Map Text (MyFormValue File)) = constDyn formData
  let eFormData = updated dFormData
  responses :: Event t XhrResponse <- postForm "/uploadprb" eFormData
  let results = view xhrResponse_responseText <$> responses

  (e1, trigger) <- newTriggerEvent

  asText <- holdDyn "No results." $ T.pack . maybe "x" show <$> results
  dynText asText
  return ()


{-
formData.append('prbText', getEditorContent(editorRef))
formData.append('prbName', documentName)
formData.append('random', randomFlag)
formData.append('outFlag', outputType)
formData.append('submit1', 'putDatabase') // temporary
-}

