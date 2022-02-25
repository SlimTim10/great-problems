module Problem.Figures
  ( widget
  ) where

import Common.Lib.Prelude

import qualified Data.Map as Map
import qualified GHCJS.DOM.File
import qualified GHCJS.DOM.Types
import qualified Language.Javascript.JSaddle as JS
import qualified Reflex.Dom.Core as R

import qualified Problem.FormFile as FormFile
import qualified Widget.Button as Button

widget
  :: ( R.DomBuilder t m
     , R.PostBuild t m
     , R.MonadHold t m
     , MonadFix m
     , R.PerformEvent t m
     , JS.MonadJSM (R.Performable m)
     )
  => R.Dynamic t [FormFile.FormFile]
  -> m (R.Dynamic t [FormFile.FormFile])
widget initialFiles = R.el "div" $ do
  fi <- R.elClass "div" "flex gap-3" $ do
    R.elClass "p" "font-medium mb-2 py-1" $ R.text "Figures"
    R.elClass "div" "w-min h-min" $ do
      R.el "label" $ mdo
        R.elClass
          "p"
          "bg-brand-primary rounded text-white font-medium px-2 py-1 text-brand-sm cursor-pointer active:bg-blue-400"
          $ R.text "Upload"
        fi1 <- R.inputElement $ R.def
          & R.initialAttributes .~ (
          "type" =: "file"
          <> "accept" =: ".asc"
          <> "multiple" =: ""
          <> "class" =: "hidden"
          )
          & R.inputElementConfig_setValue .~
          (const "" <$> R.updated (R._inputElement_files fi1))
        return fi1
  figuresWidget initialFiles (R._inputElement_files fi)

figuresWidget
  :: forall t m.
     ( R.DomBuilder t m
     , R.MonadHold t m
     , R.PostBuild t m
     , MonadFix m
     , R.PerformEvent t m
     , JS.MonadJSM (R.Performable m)
     )
  => R.Dynamic t [FormFile.FormFile]
  -> R.Dynamic t [GHCJS.DOM.Types.File]
  -> m (R.Dynamic t [FormFile.FormFile])
figuresWidget initialFiles inputFiles = do
  rec
    inputFormFiles :: R.Event t [FormFile.FormFile] <- R.performEvent
      $ R.ffor (R.updated inputFiles) $ \fs -> do
      names :: [Text] <- mapM GHCJS.DOM.File.getName fs
      return $ map (uncurry FormFile.FormFile) $ zip fs names
    deleteMap :: R.Dynamic t (Map Int (R.Event t Int)) <- R.elClass "ul" "ml-4 flex flex-col gap-2" $ do
      R.listWithKey fileMap $ \k f -> do
        R.elClass "li" "flex justify-between" $ do
          R.dynText $ FormFile.name <$> f
          fmap (const k) <$> Button.primarySmall' "Remove"
    let
      deletions :: R.Dynamic t (R.Event t (FormFile.FormFileMap -> FormFile.FormFileMap)) =
        R.mergeWith (.)
        . map (fmap Map.delete)
        . Map.elems
        <$> deleteMap
    -- Accumulate the file map (foldDyn = accumDyn . flip) by applying the (FormFile.FormFileMap -> FormFile.FormFileMap) functions.
    -- The deletions event and the new files event are merged.
    -- Note that "R.leftmost" can replace "R.mergeWith (.)",
    -- but this would not handle the two events happening at the same time
    -- (even though this may never occur in practice).
    fileMap :: R.Dynamic t FormFile.FormFileMap <- do
      R.foldDyn ($) mempty
      $ R.mergeWith (.) [R.switch . R.current $ deletions, collectFiles <$> inputFormFiles, collectFiles <$> R.updated initialFiles]
  return $ Map.elems <$> fileMap
  where
    collectFiles :: [FormFile.FormFile] -> FormFile.FormFileMap -> FormFile.FormFileMap
    collectFiles newFiles state = foldr addNew state newFiles
    addNew :: FormFile.FormFile -> FormFile.FormFileMap -> FormFile.FormFileMap
    addNew v m = case Map.maxViewWithKey m of
      Nothing -> Map.singleton 0 v
      Just ((k, _), _) -> Map.insert (succ k) v m
