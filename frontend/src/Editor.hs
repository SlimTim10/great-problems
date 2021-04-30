{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Editor
  ( editorWidget
  ) where

import Control.Monad
import Control.Monad.Fix
import Control.Lens (view)
import Data.Maybe (fromJust, maybe)
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map
import JSDOM.Types (File)
import JSDOM.File (getName)
import Language.Javascript.JSaddle (MonadJSM)

import Reflex.Dom.Core

data Options t = Options
  { random :: Dynamic t Bool
  , output :: Dynamic t Text
  , files :: Dynamic t [FileWithName]
  }

data FileWithName = FileWithName
  { file :: File
  , name :: Text
  }

instance Show FileWithName where
  show = show . name

instance Eq FileWithName where
  (==) a b = name a == name b

showText :: Show s => s -> Text
showText = T.pack . show

editorWidget
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     , MonadJSM m
     , MonadJSM (Performable m)
     , HasJSContext (Performable m)
     , PerformEvent t m
     , TriggerEvent t m
     )
  => m ()
editorWidget = do
  options :: Options t <- optionsWidget
  convertWidget options

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
        -- return $ (\(r1 :: Bool, o1, fs1) -> Options r1 o1 fs1) <$> (r, o, fs)

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
      "type" =: "file" <>
      "accept" =: ".asc" <>
      "multiple" =: ""
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
      return $ FileWithName f n

randomOption :: (DomBuilder t m, PostBuild t m) => m (Dynamic t Bool)
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
  return $ _inputElement_checked cb
  where
    randomState True = "Randomize variables is checked"
    randomState _ = "Randomize variables is not checked"

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
  let selItem = result <$> value dd
  el "p" $ do
    dynText selItem
  return $ value dd
  where
    items :: Map Text Text
    items = Map.fromList
      [ ("flagSolutions", "with solutions")
      , ("flagAnswers", "with answer")
      , ("flagSolAns", "with solution and answer")
      , ("flagQuestions", "questions only")
      ]
    result key = "You selected: " <> fromJust (Map.lookup key items)

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
  => Options t
  -> m ()
convertWidget options = el "div" $ do
  evConvert :: Event t () <- button "Convert"
  let evFormData :: Event t [Map Text (FormValue File)] = pushAlways (const buildFormData) evConvert
  responses :: Event t [XhrResponse] <- postForms "/uploadprb" evFormData
  let results = map (view xhrResponse_responseText) <$> responses
  asText <- holdDyn "No results." $ T.pack . concat . map (maybe "" show) <$> results
  dynText asText
  where
    buildFormData :: PushM t [Map Text (FormValue File)]
    buildFormData = do
      r <- sample . current $ random options
      o <- sample . current $ output options
      fs <- sample . current $ files options
      let
        formDataText :: Map Text (FormValue File) = (
          "prbText" =: FormValue_Text "editor content here" <>
          "prbName" =: FormValue_Text "untitled" <>
          "random" =: FormValue_Text (showText r) <>
          "outFlag" =: FormValue_Text (showText o) <>
          "submit1" =: FormValue_Text "putDatabase" -- temporary
          )
        formDataFiles :: Map Text (FormValue File) = Map.fromList $ flip map fs $ \f ->
          let
            fname = name f
            fval = FormValue_File (file f) (Just . name $ f)
          in (fname, fval)
      let formData = Map.unions [formDataText, formDataFiles]
      sample . current . constDyn $ [formData]
