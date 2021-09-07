module Problem
  ( widget
  ) where

import qualified Language.Javascript.JSaddle as JS
import qualified Reflex.Dom.Core as R

import qualified Common.File
import qualified Common.Compile
import qualified Problem.SelectTopic as SelectTopic
import qualified Problem.Summary as Summary
import qualified Problem.Figures as Figures
import qualified Problem.Editor as Editor
import qualified Problem.ErrorsToggle as ErrorsToggle
import qualified Problem.PdfViewer as PdfViewer
import qualified Problem.Compile as Compile
import qualified Problem.UploadPrb as UploadPrb
import qualified Problem.DownloadPrb as DownloadPrb
import qualified Widget.Button as Button
import qualified Widget.Input as Input
import qualified Util
import Global

data WithLoading a = WithLoading
  { action :: a
  , loading :: Bool
  }

widget
  :: ( R.DomBuilder t m
     , R.PostBuild t m
     , R.MonadHold t m
     , MonadFix m
     , JS.MonadJSM m
     , JS.MonadJSM (R.Performable m)
     , R.HasJSContext (R.Performable m)
     , R.PerformEvent t m
     , R.TriggerEvent t m
     )
  => m ()
widget = mdo
  ( figures
    , randomizeVariablesAction
    , resetVariablesAction
    , showAnswer
    , showSolution
    ) <- leftPane editorContent prbName
    
  ( editorContent
    , prbName
    ) <- mainPane figures randomizeVariablesAction resetVariablesAction

  return ()

  where
    leftPane editorContent prbName = do
      R.elClass "div" "w-96 flex-none flex flex-col pr-2 border-r border-brand-light-gray" $ mdo
        selectedTopicId :: R.Dynamic t Integer <- R.elClass "div" "pb-3 border-b border-brand-light-gray"
          $ SelectTopic.widget
        summary :: R.Dynamic t Text <- R.elClass "div" "py-3 border-b border-brand-light-gray"
          $ Summary.widget
        ( randomizeVariablesAction
          , resetVariablesAction
          , showAnswer
          , showSolution
          ) <- outputOptionsPane editorContent prbName figures
        figures :: R.Dynamic t [Common.File.FileWithName] <- R.elClass "div" "py-3 border-b border-brand-light-gray"
          $ Figures.widget
        publish :: R.Event t () <- R.elClass "div" "py-3" $ do
          Button.primaryClass' "Save & Publish" "w-full active:bg-blue-400"
        R.performEvent_ $ R.ffor publish $ \_ -> do
          Util.consoleLog ("Publish" :: Text)
        return (figures, randomizeVariablesAction, resetVariablesAction, showAnswer, showSolution)

    outputOptionsPane editorContent prbName figures = do
      R.elClass "div" "py-3 border-b border-brand-light-gray" $ do
        (randomizeVariablesAction, resetVariablesAction) <- R.elClass "div" "flex gap-2 mb-2" $ do
          randomizeVariables :: R.Event t () <- Button.primarySmallClass' "Randomize variables" "active:bg-blue-400"
          randomizeVariablesAction :: R.Dynamic t (WithLoading (Maybe Common.Compile.CompileResponse)) <- do
            (fmap . fmap $ uncurry WithLoading) $ do
              Compile.performRequest randomizeVariables $ Common.Compile.CompileRequest
                <$> editorContent
                <*> prbName
                <*> R.constDyn True
                <*> R.constDyn Common.Compile.WithSolutionAndAnswer -- TODO: use checkboxes
                <*> figures
          resetVariables :: R.Event t () <- Button.primarySmallClass' "Reset variables" "active:bg-blue-400"
          resetVariablesAction :: R.Dynamic t (WithLoading (Maybe Common.Compile.CompileResponse)) <- do
            (fmap . fmap $ uncurry WithLoading) $ do
              Compile.performRequest resetVariables $ Common.Compile.CompileRequest
                <$> editorContent
                <*> prbName
                <*> R.constDyn False
                <*> R.constDyn Common.Compile.WithSolutionAndAnswer -- TODO: use checkboxes
                <*> figures
          return (randomizeVariablesAction, resetVariablesAction)
        (showAnswer, showSolution) <- R.elClass "div" "flex gap-4" $ do
          R.elClass "p" "font-medium text-brand-primary"
            $ R.text "Show problem with:"
          R.elClass "div" "flex flex-col" $ do
            showAnswer :: R.Dynamic t Bool <- Input.checkboxClass
              "cursor-pointer mr-2 checkbox-brand-primary"
              "font-medium text-brand-primary cursor-pointer"
              "Answer"
            showSolution :: R.Dynamic t Bool <- Input.checkboxClass
              "cursor-pointer mr-2 checkbox-brand-primary"
              "font-medium text-brand-primary cursor-pointer"
              "Solution"
            return (showAnswer, showSolution)
        return (randomizeVariablesAction, resetVariablesAction, showAnswer, showSolution)

    mainPane figures randomizeVariablesAction resetVariablesAction = do
      R.elClass "div" "pl-2 flex-1 h-full flex flex-col" $ mdo
        (uploadPrb, prbName, compileButtonAction, errorsToggle) <- upperPane editorContent figures
        editorContent <- R.elClass "div" "h-full flex" $ do
          editorContent' :: R.Dynamic t Text <- R.elClass "div" "flex-1" $ Editor.widget uploadPrb
          R.elClass "div" "flex-1" $ do
            let actions = [compileButtonAction, randomizeVariablesAction, resetVariablesAction]
            anyResponse :: R.Dynamic t (Maybe Common.Compile.CompileResponse) <- R.holdDyn Nothing
              $ R.leftmost . map (R.updated . fmap action)
              $ actions
            anyLoading :: R.Dynamic t Bool <- R.holdDyn False
              $ R.leftmost . map (R.updated . fmap loading)
              $ actions
            PdfViewer.widget anyResponse anyLoading errorsToggle
          return editorContent'
        return (editorContent, prbName)

    upperPane editorContent figures = do
      R.elClass "div" "bg-brand-light-gray p-1 flex justify-between" $ do
        (uploadPrb, prbName) <- do
          R.elClass "div" "flex-1 flex justify-center" $ do
            R.elClass "span" "mr-auto flex gap-2 items-center" $ mdo
              uploadPrb :: R.Event t Text <- UploadPrb.widget
              DownloadPrb.widget prbName editorContent
              R.elClass "p" "ml-2" $ R.text "Filename"
              prbName :: R.Dynamic t Text <- fmap R.value $ R.inputElement
                $ R.def & R.inputElementConfig_elementConfig . R.elementConfig_initialAttributes .~
                ("type" =: "text" <> "class" =: "pl-1")
                & R.inputElementConfig_initialValue .~ "untitled"
              return (uploadPrb, prbName)
        compileButtonAction :: R.Dynamic t (WithLoading (Maybe Common.Compile.CompileResponse)) <- do
          (fmap . fmap $ uncurry WithLoading) $ do
            R.elClass "div" "flex-1 flex justify-center" $ do
              Compile.widget $ Common.Compile.CompileRequest
                <$> editorContent
                <*> prbName
                <*> R.constDyn False
                <*> R.constDyn Common.Compile.QuestionOnly -- TODO: use checkboxes
                <*> figures
        errorsToggle :: R.Dynamic t Bool <- R.elClass "div" "flex-1 flex justify-center ml-auto" $ do
          R.elClass "span" "ml-auto" $ ErrorsToggle.widget (action <$> compileButtonAction) (R.updated $ loading <$> compileButtonAction)
        return (uploadPrb, prbName, compileButtonAction, errorsToggle)
