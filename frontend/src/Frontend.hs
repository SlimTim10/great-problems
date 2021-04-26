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
import qualified Data.Text.Encoding as T
import qualified Data.Map as Map
import Language.Javascript.JSaddle (eval, liftJSM, syncPoint, valToText, MonadJSM, toJSVal)
import JSDOM.Types (unFile, fromJSVal, JSVal(..), File, FromJSString)
import JSDOM.File (getName)
import Data.Maybe (fromJust, listToMaybe)
import qualified GHCJS.DOM.HTMLInputElement as Input
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.IO.Class (liftIO)
import           GHCJS.DOM.FileReader  (newFileReader, readAsDataURL, load
                                       , getResult)
import GHCJS.DOM.EventM (on)
import Data.Witherable (Filterable)

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

      prerender_ blank $ options

      -- myTest

      -- tutorial8
  }

options :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, MonadJSM m, Prerender js t m) => m ()
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
  :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, MonadJSM m)
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
  -- let drawings = value fi
  -- drawing <- (return . fmap headMay) drawings
  let dynState = drawingsState <$> _inputElement_files fi -- :: Dynamic t T.Text
  -- let dynState = T.pack . show . map ((show :: JSVal -> String) . unFile) <$> _inputElement_files fi
  dynText dynState
  drawingsWidget $ _inputElement_files fi

  -- domEvent Change fi1
  
  return ()

drawingsWidget
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m, MonadJSM m)
  => Dynamic t [File]
  -> m (Dynamic t [()])
drawingsWidget drawingsDyn = simpleList drawingsDyn drawingWidget

drawingWidget
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m, MonadJSM m)
  => Dynamic t File
  -> m ()
drawingWidget drawingDyn = el "div" $ do
  -- let dynRes = getName <$> drawingDyn -- :: (MonadJSM m, FromJSString result) => Dynamic t (m result)
  
  -- mRes <- dynRes :: (MonadJSM m, FromJSString result) => m result
  -- name :: Dynamic t T.Text <- r
  -- void $ dyn $ getName <$> drawingDyn
  -- let name = fromJSString <$> getName drawingDyn
  -- dynText name
  -- display r
  -- performEvent $ dynRes
  
  -- getNameAction <- (return . (getNameText <$>)) drawingDyn
  -- filename <- (return . (unsafePerformIO <$>)) getNameAction
  -- dynText (T.pack . show <$> filename)

  -- filesDyn

  -- WORKING!
  getNameAction <- (return . dyn . (getNameText <$>)) drawingDyn
  evText <- getNameAction
  x <- holdDyn "" evText
  dynText $ x

  return ()
  where
    getNameText :: MonadJSM m => File -> m T.Text
    getNameText = getName

go2
  ::
    ( DomBuilder t m
    , MonadHold t m
    , PostBuild t m
    , MonadFix m
    , MonadJSM m
    , FromJSString (Event t File)
    )
  => Dynamic t File
  -> m (Event t File)
go2 fileDyn = do
  let mapped = getName <$> fileDyn
  dyned <- dyn mapped
  held <- hold never dyned
  return $ switch held

instance Show File where
  show file = "test"

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

buttonClass :: DomBuilder t m => T.Text -> T.Text -> m (Event t ())
buttonClass c s = do
  (e, _) <- elAttr' "button" ("type" =: "button" <> "class" =: c) $ text s
  return $ domEvent Click e

numberPad :: (DomBuilder t m) => m (Event t T.Text)
numberPad = do
  b7 <- ("7" <$) <$> numberButton "7"
  b8 <- ("8" <$) <$> numberButton "8"
  b9 <- ("9" <$) <$> numberButton "9"
  b4 <- ("4" <$) <$> numberButton "4"
  b5 <- ("5" <$) <$> numberButton "5"
  b6 <- ("6" <$) <$> numberButton "6"
  b1 <- ("1" <$) <$> numberButton "1"
  b2 <- ("2" <$) <$> numberButton "2"
  b3 <- ("3" <$) <$> numberButton "3"
  b0 <- ("0" <$) <$> buttonClass "number zero" "0"
  return $ leftmost [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9]
  where
    numberButton n = buttonClass "number" n

tutorial8 :: (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m) => m ()
tutorial8 = el "div" $ do
  numberButton <- numberPad
  clearButton <- button "C"
  let
    buttons = leftmost
      [ Nothing <$ clearButton
      , Just <$> numberButton
      ]
  dstate <- accumDyn collectButtonPresses initialState buttons
  text " "
  dynText dstate
  where
    initialState :: T.Text
    initialState = T.empty
    collectButtonPresses :: T.Text -> Maybe T.Text -> T.Text
    collectButtonPresses state buttonPress =
      case buttonPress of
        Nothing -> initialState
        Just digit -> state <> digit

-- openFileDialog
--   :: (DomBuilder t m, PostBuild t m, TriggerEvent t m, PerformEvent t m, MonadJSM (Performable m), MonadJS x0 [], FromJSString (JSRef x0))
--   => m (Event t T.Text)
-- openFileDialog = do
--   input <- inputElement $ def & initialAttributes .~ (
--     "type" =: "file" <>
--     "accept" =: ".asc" <>
--     "multiple" =: ""
--     )
--   let newFile = fmapMaybe listToMaybe $ updated $ _inputElement_files input
--   fName <- performEventAsync $ ffor newFile $ \file cb -> liftJSM $ do
--     _ <- liftJSM $ do
--       name <- T.pack . head . fromJSString <$> getName file
--       liftIO $ cb $ name
--     pure ()
--   pure fName

-- app
--   :: ( DomBuilder t m
--      , MonadHold t m
--      , PerformEvent t m
--      , TriggerEvent t m
--      , Prerender js t m
--      , Filterable (Dynamic t)
--      )
--   => m ()
-- app = do
--   filesDyn <- fileInputElement
--   urlE <- fmap (ffilter ("data:image" `T.isPrefixOf`))
--       . dataURLFileReader
--       . fmapMaybe listToMaybe
--       . updated $ filesDyn
--   el "br" blank
--   void $ el "div"
--     . widgetHold blank
--     . ffor urlE $ \url -> elAttr "img" ("src" =: url <> "style" =: "max-width: 80%") blank
--   return ()

-- fileInputElement :: DomBuilder t m => m (Dynamic t [File])
-- fileInputElement = do
--   ie <- inputElement $ def
--     & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
--       ("type" =: "file" <> "accept" =: "image/png, image/jpeg")
--   return (_inputElement_files ie)

-- dataURLFileReader
--   :: ( DomBuilder t m
--      , TriggerEvent t m
--      , PerformEvent t m
--      , Prerender js t m
--      )
--   => Event t File -> m (Event t T.Text)
-- dataURLFileReader request = prerender (return never) $ do
--   fileReader <- liftJSM newFileReader
--   performEvent_ (fmap (readAsDataURL fileReader . Just) request)
--   e <- wrapDomEvent fileReader (`on` load) . liftJSM $ do
--     v <- getResult fileReader
--     (fromJSVal <=< toJSVal) v
--   return (fmapMaybe id e)

myTest
  ::
    ( DomBuilder t m
    , PostBuild t m
    , MonadHold t m
    -- , MonadFix m
    -- , MonadJSM m
    -- , Prerender js t m
    )
  => m ()
myTest = do
  el "br" blank
  el "h2" $ text "Text Input - Read Value on Button Click"
  elInput <- inputElement def
  dynText $ _inputElement_value elInput
  el "br" blank
  dHeld <- holdDyn "" . updated $ _inputElement_value elInput
  display dHeld
  el "br" blank

  filesDyn <- fileInputElement
  display filesDyn
  
  el "br" blank

fileInputElement :: DomBuilder t m => m (Dynamic t [File])
fileInputElement = do
  ie <- inputElement $ def & initialAttributes .~ (
    "type" =: "file" <>
    "accept" =: ".asc" <>
    "multiple" =: ""
    )
  return $ _inputElement_files ie
