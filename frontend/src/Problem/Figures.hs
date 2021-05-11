module Problem.Figures
  ( widget
  , FileWithName(..)
  ) where

import qualified Control.Monad.Fix as Fix
import qualified Data.Map as Map

import qualified JSDOM.File
import qualified JSDOM.Types
import qualified Language.Javascript.JSaddle as JS
import qualified Reflex.Dom.Core as R

import Global

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
  => m (R.Dynamic t [FileWithName])
widget = do
  R.elClass "p" "text-xl bg-blue-200" $ R.text "Figures"
  fi <- R.el "label" $ do
    R.text "Upload"
    fi1 <- R.inputElement $ R.def & R.initialAttributes .~ (
      "type" =: "file"
      <> "accept" =: ".asc"
      <> "multiple" =: ""
      <> "class" =: "hidden"
      )
    return fi1
  figuresWidget $ R._inputElement_files fi

figuresWidget
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
figuresWidget figures = do
  rec
    filesWithNames :: R.Event t [FileWithName] <- R.performEvent $ R.ffor (R.updated figures) $ \fs -> do
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
    -- Accumulate the file map (foldDyn = accumDyn . flip) by applying the (FileMap -> FileMap) functions.
    -- The deletions event and the new files event are merged.
    -- Note that "R.leftmost" can replace "R.mergeWith (.)",
    -- but this would not handle the two events happening at the same time
    -- (even though this may never occur in practice).
    fileMap :: R.Dynamic t FileMap <-
      R.foldDyn ($) Map.empty
      $ R.mergeWith (.) [R.switch . R.current $ deletions, collectFiles <$> filesWithNames]
  return $ Map.elems <$> fileMap
  where
    collectFiles :: [FileWithName] -> FileMap -> FileMap
    collectFiles newFiles state = foldr addNew state newFiles
    addNew :: FileWithName -> FileMap -> FileMap
    addNew v m = case Map.maxViewWithKey m of
      Nothing -> Map.singleton 0 v
      Just ((k, _), _) -> Map.insert (succ k) v m