module Problem.Options
  ( widget
  ) where

import qualified Control.Monad.Fix as Fix
import qualified Data.List as List
import qualified Data.Map as Map

import qualified JSDOM.File
import qualified JSDOM.Types
import qualified Language.Javascript.JSaddle as JS
import qualified Reflex.Dom.Core as R

import qualified Problem.Types as Types
import Global
import Util

widget
  :: ( R.DomBuilder t m
     , R.PostBuild t m
     , R.MonadHold t m
     , Fix.MonadFix m
     , JS.MonadJSM m
     )
  => m (Types.Options t)
widget = do
  R.elAttr "div" ("style" =: "border: 1px solid black;") $ do
    R.elClass "div" "mainContainer" $ do
      R.elClass "div" "optionsContainer" $ do
        R.el "h2" $ R.text "Options"
        r :: R.Dynamic t Bool <- randomOption
        o :: R.Dynamic t Text <- outputOption
        fs :: R.Dynamic t [Types.FileWithName] <- drawingsOption
        return $ Types.Options r o fs

drawingsOption
  :: ( R.DomBuilder t m
     , R.PostBuild t m
     , R.MonadHold t m
     , Fix.MonadFix m
     , JS.MonadJSM m
     )
  => m (R.Dynamic t [Types.FileWithName])
drawingsOption = R.el "div" $ do
  R.el "h4" $ R.text "Drawings"
  fi <- R.el "label" $ do
    R.text "Upload"
    fi1 <- R.inputElement $ R.def & R.initialAttributes .~ (
      "type" =: "file"
      <> "accept" =: ".asc"
      <> "multiple" =: ""
      )
    return fi1
  fs <- drawingsWidget $ R._inputElement_files fi
  return fs

drawingsWidget
  :: forall t m.
     ( R.DomBuilder t m
     , R.MonadHold t m
     , R.PostBuild t m
     , Fix.MonadFix m
     , JS.MonadJSM m
     )
  => R.Dynamic t [JSDOM.Types.File]
  -> m (R.Dynamic t [Types.FileWithName])
drawingsWidget drawings = do
  rawFiles :: R.Dynamic t [JSDOM.Types.File] <- R.accumDyn (<>) [] (R.updated drawings)
  files :: R.Dynamic t [Types.FileWithName] <- R.simpleList rawFiles mkFile
  uniqueFiles :: R.Dynamic t [Types.FileWithName] <- R.accumDyn collectFiles [] (R.updated files)
  R.el "ul" $ do
    void $ R.simpleList uniqueFiles $ \file -> do
      R.el "li" $ do
        R.dynText $ Types.name <$> file
  return uniqueFiles

  where
    collectFiles :: [Types.FileWithName] -> [Types.FileWithName] -> [Types.FileWithName]
    collectFiles state newFiles = List.nub $ state <> newFiles

    mkFile :: R.Dynamic t JSDOM.Types.File -> m Types.FileWithName
    mkFile file = do
      let name :: R.Dynamic t (m Text) = JSDOM.File.getName <$> file
      mName :: m Text <- R.sample . R.current $ name
      f :: JSDOM.Types.File <- R.sample . R.current $ file
      n :: Text <- mName
      consoleLog ("mkFile" :: Text)
      consoleLog f
      return $ Types.FileWithName f n

randomOption :: (R.DomBuilder t m) => m (R.Dynamic t Bool)
randomOption = R.el "div" $ do
  cb <- R.el "label" $ do
    cb1 <- R.inputElement $ R.def & R.initialAttributes .~ (
      "type" =: "checkbox"
      )
    R.text "Randomize variables"
    return cb1
  return $ R._inputElement_checked cb

outputOption
  :: ( R.DomBuilder t m
     , R.PostBuild t m
     , R.MonadHold t m
     , Fix.MonadFix m
     )
  => m (R.Dynamic t Text)
outputOption = R.el "div" $ do
  dd <- R.el "label" $ do
    R.text "Output"
    dd1 <- R.dropdown "flagSolutions" (R.constDyn items) R.def
    return dd1
  return $ R.value dd
  where
    items :: Map Text Text
    items = Map.fromList
      [ ("flagSolutions", "with solutions")
      , ("flagAnswers", "with answer")
      , ("flagSolAns", "with solution and answer")
      , ("flagQuestions", "questions only")
      ]
