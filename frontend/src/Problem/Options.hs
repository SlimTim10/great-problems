module Problem.Options
  ( widget
  , Options(..)
  , FileWithName(..)
  ) where

import qualified Control.Monad.Fix as Fix
import qualified Data.Map as Map

import qualified JSDOM.File
import qualified JSDOM.Types
import qualified Language.Javascript.JSaddle as JS
import qualified Reflex.Dom.Core as R

import Global

data Options = Options
  { random :: Bool
  , output :: Text
  , files :: [FileWithName]
  }

type FileMap = Map.Map Int FileWithName

data FileWithName = FileWithName
  { file :: JSDOM.Types.File
  , name :: Text
  }

instance Show FileWithName where
  show = show . name

instance Eq FileWithName where
  (==) a b = name a == name b

widget
  :: ( R.DomBuilder t m
     , R.PostBuild t m
     , R.MonadHold t m
     , Fix.MonadFix m
     , R.PerformEvent t m
     , JS.MonadJSM (R.Performable m)
     )
  => m (R.Dynamic t Options)
widget = do
  R.elAttr "div" ("style" =: "border: 1px solid black;") $ do
    R.elClass "div" "mainContainer" $ do
      R.elClass "div" "optionsContainer" $ do
        R.el "h2" $ R.text "Options"
        r :: R.Dynamic t Bool <- randomOption
        o :: R.Dynamic t Text <- outputOption
        ds :: R.Dynamic t [FileWithName] <- drawingsOption
        let options = Options <$> r <*> o <*> ds
        R.holdDyn (Options False "" []) $ R.updated options

drawingsOption
  :: ( R.DomBuilder t m
     , R.PostBuild t m
     , R.MonadHold t m
     , Fix.MonadFix m
     , R.PerformEvent t m
     , JS.MonadJSM (R.Performable m)
     )
  => m (R.Dynamic t [FileWithName])
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
  drawingsWidget $ R._inputElement_files fi

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
  -> m (R.Dynamic t [FileWithName])
drawingsWidget drawings = do
  rec
    filesWithNames :: R.Event t [FileWithName] <- R.performEvent $ R.ffor (R.updated drawings) $ \fs -> do
      names :: [Text] <- mapM JSDOM.File.getName fs
      return $ map (uncurry FileWithName) $ zip fs names
    deleteMap :: R.Dynamic t (Map Int (R.Event t Int)) <- R.el "ul" $ do
      R.listWithKey fileMap $ \k f -> do
        R.el "li" $ do
          R.dynText $ name <$> f
          fmap (const k) <$> R.button "X"
    let
      deletions :: R.Dynamic t (R.Event t (FileMap -> FileMap)) =
        R.mergeWith (.)
        . map (fmap Map.delete)
        . Map.elems
        <$> deleteMap
    -- Accumulate the file map (foldDyn = accumDyn . flip),
    -- merging the deletions event and the new files event.
    -- Note that "R.leftmost" can replace "R.mergeWith (.)",
    -- but this would not handle the two events happening at the same time.
    fileMap :: R.Dynamic t FileMap <-
      R.foldDyn id Map.empty
      $ R.mergeWith (.) [R.switch . R.current $ deletions, collectFiles <$> filesWithNames]
  return $ Map.elems <$> fileMap
  where
    collectFiles :: [FileWithName] -> FileMap -> FileMap
    collectFiles newFiles state = foldr addNew state newFiles
    addNew :: FileWithName -> FileMap -> FileMap
    addNew v m = case Map.maxViewWithKey m of
      Nothing -> Map.singleton 0 v
      Just ((k, _), _) -> Map.insert (succ k) v m

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
