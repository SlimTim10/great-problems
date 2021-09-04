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

import qualified Widget.Button as Button
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
widget = R.el "div" $ do
  fi <- R.elClass "div" "flex gap-3" $ do
    R.elClass "p" "font-medium mb-2 py-1" $ R.text "Figures"
    R.elClass "div" "w-min h-min" $ do
      R.el "label" $ do
        R.elClass
          "p"
          "bg-brand-primary rounded text-white font-medium px-2 py-1 text-brand-sm cursor-pointer"
          $ R.text "Upload"
        R.inputElement $ R.def & R.initialAttributes .~ (
          "type" =: "file"
          <> "accept" =: ".asc"
          <> "multiple" =: ""
          <> "class" =: "hidden"
          )
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
    deleteMap :: R.Dynamic t (Map Int (R.Event t Int)) <- R.elClass "ul" "ml-4 flex flex-col gap-2" $ do
      R.listWithKey fileMap $ \k f -> do
        R.elClass "li" "flex justify-between" $ do
          R.dynText $ name <$> f
          fmap (const k) <$> Button.primarySmall' "Remove"
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
