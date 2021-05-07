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

widget
  :: ( R.DomBuilder t m
     , R.PostBuild t m
     , R.MonadHold t m
     , Fix.MonadFix m
     , R.PerformEvent t m
     , JS.MonadJSM (R.Performable m)
     )
  => m (R.Dynamic t Types.Options)
widget = do
  R.elAttr "div" ("style" =: "border: 1px solid black;") $ do
    R.elClass "div" "mainContainer" $ do
      R.elClass "div" "optionsContainer" $ do
        R.el "h2" $ R.text "Options"
        random :: R.Dynamic t Bool <- randomOption
        output :: R.Dynamic t Text <- outputOption
        drawings :: R.Dynamic t [Types.FileWithName] <- drawingsOption
        let options = Types.Options <$> random <*> output <*> drawings
        ops <- R.holdDyn (Types.Options False "" []) $ R.updated options
        return ops

drawingsOption
  :: ( R.DomBuilder t m
     , R.PostBuild t m
     , R.MonadHold t m
     , Fix.MonadFix m
     , R.PerformEvent t m
     , JS.MonadJSM (R.Performable m)
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
     , R.PerformEvent t m
     , JS.MonadJSM (R.Performable m)
     )
  => R.Dynamic t [JSDOM.Types.File]
  -> m (R.Dynamic t [Types.FileWithName])
drawingsWidget drawings = do
  files :: R.Event t [Types.FileWithName] <- R.performEvent $ R.ffor (R.updated drawings) $ \fs -> do
    names :: [Text] <- mapM JSDOM.File.getName fs
    return $ map (uncurry Types.FileWithName) $ zip fs names
  uniqueFiles :: R.Dynamic t [Types.FileWithName] <- R.accumDyn collectFiles [] files
  R.el "ul" $ do
    void $ R.simpleList uniqueFiles $ \file -> do
      R.el "li" $ do
        R.dynText $ Types.name <$> file
  return uniqueFiles
  where
    collectFiles :: [Types.FileWithName] -> [Types.FileWithName] -> [Types.FileWithName]
    collectFiles state newFiles = List.nub $ state <> newFiles

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
