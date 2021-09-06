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
widget = do
  (figures, randomizeVariables, resetVariables, showAnswer, showSolution) <- do
    R.elClass "div" "w-96 flex-none flex flex-col pr-2 border-r border-brand-light-gray" $ do
    
      selectedTopicId :: R.Dynamic t Integer <- R.elClass "div" "pb-3 border-b border-brand-light-gray" $
        SelectTopic.widget
      
      summary :: R.Dynamic t Text <- R.elClass "div" "py-3 border-b border-brand-light-gray" $
        Summary.widget
      
      ( randomizeVariables :: R.Event t ()
        , resetVariables :: R.Event t ()
        , showAnswer :: R.Dynamic t Bool
        , showSolution :: R.Dynamic t Bool
        ) <-
        R.elClass "div" "py-3 border-b border-brand-light-gray" $ do
          (randomizeVariables, resetVariables) <- R.elClass "div" "flex gap-2 mb-2" $ do
            randomizeVariables :: R.Event t () <- Button.primarySmallClass' "Randomize variables" "active:bg-blue-400"
            resetVariables :: R.Event t () <- Button.primarySmallClass' "Reset variables" "active:bg-blue-400"
            return (randomizeVariables, resetVariables)
          (showAnswer, showSolution) <- R.elClass "div" "flex gap-4" $ do
            R.elClass "p" "font-medium text-brand-primary" $ R.text "Show problem with:"
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
          return (randomizeVariables, resetVariables, showAnswer, showSolution)

      figures :: R.Dynamic t [Common.File.FileWithName] <- R.elClass "div" "py-3 border-b border-brand-light-gray" $
        Figures.widget

      publish :: R.Event t () <- R.elClass "div" "py-3" $ do
        Button.primaryClass' "Save & Publish" "w-full active:bg-blue-400"

      R.performEvent_ $ R.ffor publish $ \_ -> do
        Util.consoleLog ("Publish" :: Text)

      return (figures, randomizeVariables, resetVariables, showAnswer, showSolution)
      
  R.elClass "div" "pl-2 flex-1 h-full flex flex-col" $ mdo
    (uploadPrb, loading, compileResponse, errorsToggle, compileResponse''', loading''') <- do
      R.elClass "div" "bg-brand-light-gray p-1 flex justify-between" $ do
        (uploadPrb', prbName) <- do
          R.elClass "div" "flex-1 flex justify-center" $ do
            R.elClass "span" "mr-auto flex gap-2 items-center" $ mdo
              uploadPrb :: R.Event t Text <- UploadPrb.widget
              DownloadPrb.widget prbName editorContent
              R.elClass "p" "ml-2" $ R.text "Filename"
              prbName :: R.Dynamic t Text <- fmap R.value $ R.inputElement $
                R.def & R.inputElementConfig_elementConfig . R.elementConfig_initialAttributes .~
                ("type" =: "text" <> "class" =: "pl-1")
                & R.inputElementConfig_initialValue .~ "untitled"
              return (uploadPrb, prbName)

        -- Do the compile for randomizeVariables button
        ( compileResponse'''' :: R.Dynamic t (Maybe Common.Compile.CompileResponse)
          , loading'''' :: R.Dynamic t Bool) <- fmap R.splitDynPure $
          Compile.performRequest randomizeVariables $ Common.Compile.CompileRequest
            <$> editorContent
            <*> prbName
            <*> R.constDyn True
            <*> R.constDyn Common.Compile.WithSolutionAndAnswer -- TODO: use checkboxes
            <*> figures

        ( compileResponse' :: R.Dynamic t (Maybe Common.Compile.CompileResponse)
          , loading' :: R.Dynamic t Bool
          ) <- fmap R.splitDynPure $
          R.elClass "div" "flex-1 flex justify-center" $ do
            Compile.widget $ Common.Compile.CompileRequest
              <$> editorContent
              <*> prbName
              <*> R.constDyn False
              <*> R.constDyn Common.Compile.QuestionOnly -- TODO: use checkboxes
              <*> figures
        errorsToggle' :: R.Dynamic t Bool <- R.elClass "div" "flex-1 flex justify-center ml-auto" $ do
          R.elClass "span" "ml-auto" $ ErrorsToggle.widget compileResponse (R.updated loading)
        return (uploadPrb', loading', compileResponse', errorsToggle', compileResponse'''', loading'''')
    editorContent <- R.elClass "div" "h-full flex" $ do
      editorContent' :: R.Dynamic t Text <- R.elClass "div" "flex-1" $ Editor.widget uploadPrb
      R.elClass "div" "flex-1" $ do
        x :: R.Dynamic t (Maybe Common.Compile.CompileResponse) <-
          R.holdDyn Nothing $
          R.leftmost [R.updated compileResponse, R.updated compileResponse''']
        PdfViewer.widget x loading errorsToggle
      return editorContent'
      
    return ()

    -- (randomizeVariables, resetVariables, outputOption) :: (R.Event t (), R.Event t (), R.Dynamic t Text) <-
    --   R.elClass "div" "py-2 border-b border-brand-light-gray" $
    --   Options.widget

  -- R.elClass "div" "flex-1 h-full flex gap-4" $ do

  --   (options, figures) <- R.elClass "div" "flex-none w-56 flex flex-col gap-4" $ do
  --     options <- R.elClass "div" "border-2 border-gray-300" $ Options.widget
  --     figures <- R.elClass "div" "h-full border-2 border-gray-300" $ Figures.widget
  --     return (options, figures)
      
  --   R.elClass "div" "flex-1 h-full flex flex-col" $ mdo
      
  --     (uploadPrb, convertResponse, loading, errorsToggle) <- R.elClass "div" "bg-gray-100 flex justify-between" $ mdo
  --       uploadPrb <- UploadPrb.widget
  --       DownloadPrb.widget prbName editorContent
  --       prbName <- prbNameWidget
  --       (convertResponse, loading) <- Convert.widget options figures prbName editorContent
  --         <&> R.splitDynPure
  --       errorsToggle <- ErrorsToggle.widget convertResponse (R.updated loading)
  --       return (uploadPrb, convertResponse, loading, errorsToggle)
        
  --     editorContent <- R.elClass "div" "h-full flex" $ mdo
  --       editorContent <- R.elClass "div" "h-full flex-1"$ Editor.widget uploadPrb
  --       let pdfData = maybe "" Convert.pdfContent <$> convertResponse
  --       R.elClass "div" "flex-1" $ PdfViewer.widget pdfData loading convertResponse errorsToggle
  --       return editorContent
        
  --     return ()
