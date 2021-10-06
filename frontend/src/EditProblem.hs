{-# LANGUAGE PackageImports #-}
module EditProblem
  ( widget
  ) where

import qualified Data.Map as Map
import qualified Language.Javascript.JSaddle as JS
import qualified "jsaddle-dom" GHCJS.DOM.Document as DOM
import qualified JSDOM.Types
import qualified Obelisk.Route.Frontend as Ob
import qualified Reflex.Dom.Core as R
-- Import patch
import qualified MyReflex.Dom.Xhr.FormData as R'

import qualified Common.Route as Route
import qualified Common.File
import qualified Common.Api.Compile as Compile
import qualified Common.Api.Error as Error
import qualified Common.Api.Problem as Problem
import qualified Common.Api.Topic as Topic
import qualified Common.Api.User as User
import qualified Problem.SelectTopic as SelectTopic
import qualified Problem.Summary as Summary
import qualified Problem.Figures as Figures
import qualified Problem.Editor as Editor
import qualified Problem.ErrorsToggle as ErrorsToggle
import qualified Problem.PdfViewer as PdfViewer
import qualified Problem.Compile
import qualified Problem.UploadPrb as UploadPrb
import qualified Problem.DownloadPrb as DownloadPrb
import qualified Problem.Loading as Loading
import qualified Widget.Button as Button
import qualified Widget.Input as Input
import qualified Util
import Global

widget
  :: forall t m js.
     ( R.DomBuilder t m
     , R.PostBuild t m
     , R.MonadHold t m
     , MonadFix m
     , JS.MonadJSM m
     , JS.MonadJSM (R.Performable m)
     , R.HasJSContext (R.Performable m)
     , R.PerformEvent t m
     , R.TriggerEvent t m
     , R.MonadSample t (R.Performable m)
     , DOM.IsDocument (R.RawDocument (R.DomBuilderSpace m))
     , R.HasDocument m
     , Ob.RouteToUrl (Ob.R Route.FrontendRoute) m
     , Ob.SetRoute t (Ob.R Route.FrontendRoute) m
     , R.Prerender js t m
     )
  => Maybe Integer
  -> m ()
widget problemId = mdo
  preloadedProblem :: R.Dynamic t (Maybe Problem.Problem) <- do
    response :: R.Event t (Maybe Problem.Problem) <- case problemId of
      Nothing -> return R.never
      Just pid -> do
        Util.getOnload
          $ Route.apiHref $ Route.Api_Problems :/ (Just pid, mempty)
    R.holdDyn Nothing response
  
  ( figures
    , randomizeVariablesAction
    , resetVariablesAction
    , showAnswerAction
    , showSolutionAction
    , outputOption
    ) <- leftPane preloadedProblem editorContent
    
  ( editorContent
    , compileButtonAction
    ) <- mainPane
    preloadedProblem
    figures
    latestResponse
    anyLoading
    outputOption

  let actions =
        [ compileButtonAction
        , randomizeVariablesAction
        , resetVariablesAction
        , showAnswerAction
        , showSolutionAction
        ]

  -- Only the latest response matters and it is what we show
  latestResponse :: R.Dynamic t (Maybe Compile.Response) <- R.holdDyn Nothing
    $ Loading.latestAction actions

  -- If any response is still loading, we want to show the loading spinner
  anyLoading :: R.Dynamic t Bool <- Loading.anyLoading actions

  return ()

  where
    leftPane preloadedProblem editorContent = do
      R.elClass "div" "w-96 flex-none flex flex-col pr-2 border-r border-brand-light-gray" $ mdo
        when (isJust problemId) $ do
          R.elClass "div" "pb-3 border-b border-brand-light-gray" $ do
            R.elClass "p" "font-medium mb-2" $ R.text "View"
            Ob.routeLink
              (Route.FrontendRoute_Problems :/
                (fromJust problemId, Route.ProblemsRoute_View :/ ())) $ do
              R.elClass "p" "text-brand-primary font-medium hover:underline" $ do
                R.text "Click here to view this problem"
          R.elClass "div" "pb-3" R.blank
        let setTopicId :: R.Event t Integer =
              fromMaybe 0
              . fmap (either id Topic.id . Problem.topic)
              <$> R.updated preloadedProblem
        selectedTopicId :: R.Dynamic t Integer <- R.elClass "div" "pb-3 border-b border-brand-light-gray"
          $ SelectTopic.widget setTopicId
        let setSummaryValue :: R.Event t Text = fromMaybe "" . fmap Problem.summary
              <$> R.updated preloadedProblem
        summary :: R.Dynamic t Text <- R.elClass "div" "py-3 border-b border-brand-light-gray"
          $ Summary.widget setSummaryValue
              
        ( randomizeVariablesAction
          , resetVariablesAction
          , showAnswerAction
          , showSolutionAction
          , outputOption
          ) <- outputOptionsPane editorContent figures
        figures :: R.Dynamic t [Common.File.FileWithName] <- R.elClass "div" "py-3 border-b border-brand-light-gray"
          $ Figures.widget
        publish :: R.Event t () <- R.elClass "div" "py-3" $ do
          Button.primaryClass' "Save & Publish" "w-full active:bg-blue-400"
        let isEditing = isJust problemId
        finishMessage <- R.holdDyn R.blank $ saveResponse <&> \case
          Nothing -> R.elClass "p" "text-red-500" $ R.dynText errorMessage
          Just savedProblem -> case isEditing of
            True -> R.elClass "p" "text-green-600" $ R.text "Saved!"
            False -> do
              R.el "p" $ R.text "Published!"
              Ob.routeLink
                (Route.FrontendRoute_Problems :/
                  (Problem.id savedProblem, Route.ProblemsRoute_View :/ ())) $ do
                R.elClass "p" "text-brand-primary font-medium hover:underline" $ do
                  R.text "Click here to view your published problem"
              timer :: R.Event t R.TickInfo <- R.tickLossyFromPostBuildTime 0.01
              Ob.setRoute $ do
                (Route.FrontendRoute_Problems :/
                 (Problem.id savedProblem, Route.ProblemsRoute_Edit :/ ())) <$ timer
        savingMessage <- R.performEvent $ R.ffor publish $ \_ -> do
          return $ R.el "p" $ R.text "Saving..."
        message <- R.holdDyn R.blank $ R.leftmost [savingMessage, R.updated finishMessage]
        R.dyn_ message

        userId :: R.Dynamic t Integer <-
          pure
          . R.constDyn
          . fromMaybe 0
          . fmap User.id
          =<< Util.getCurrentUser
          
        let saveProblemRequest :: R.Dynamic t Problem.RequestSave = case problemId of
              Nothing -> Problem.RequestSave
                <$> (
                fmap Left
                  $ Problem.CreateProblem
                  <$> summary
                  <*> editorContent
                  <*> selectedTopicId
                  <*> userId
                )
                <*> figures
              Just pid -> Problem.RequestSave
                <$> (
                fmap Right
                  $ Problem.UpdateProblem
                  <$> R.constDyn pid
                  <*> summary
                  <*> editorContent
                  <*> selectedTopicId
                  <*> userId
                )
                <*> figures
              
        formData :: R.Event t (Map Text (R'.FormValue JSDOM.Types.File)) <- R.performEvent
          $ R.ffor (R.tagPromptlyDyn saveProblemRequest publish) $ \req -> do
          let
            formDataParams :: Map Problem.RequestParam (R'.FormValue JSDOM.Types.File) =
              case Problem.rsProblem req of
                Left createProblem' -> (
                  Problem.ParamSummary =: R'.FormValue_Text (Problem.cpSummary createProblem')
                  <> Problem.ParamContent =: R'.FormValue_Text (Problem.cpContent createProblem')
                  <> Problem.ParamTopicId =: R'.FormValue_Text
                    (cs . show . Problem.cpTopicId $ createProblem')
                  <> Problem.ParamAuthorId =: R'.FormValue_Text
                    (cs . show . Problem.cpAuthorId $ createProblem')
                  <> Problem.ParamFigures =: R'.FormValue_List (map Util.formFile . Problem.rsFigures $ req)
                  )
                Right updateProblem' -> (
                  Problem.ParamProblemId =: R'.FormValue_Text
                    (cs . show . Problem.upProblemId $ updateProblem')
                  <> Problem.ParamSummary =: R'.FormValue_Text (Problem.upSummary updateProblem')
                  <> Problem.ParamContent =: R'.FormValue_Text (Problem.upContent updateProblem')
                  <> Problem.ParamTopicId =: R'.FormValue_Text
                    (cs . show . Problem.upTopicId $  updateProblem')
                  <> Problem.ParamAuthorId =: R'.FormValue_Text
                    (cs . show . Problem.upAuthorId $ updateProblem')
                  <> Problem.ParamFigures =: R'.FormValue_List (map Util.formFile . Problem.rsFigures $ req)
                  )
            formDataText = Map.mapKeys (cs . show) formDataParams
          return formDataText
        response :: R.Event t Text <- Util.postForm
          (Route.apiHref $ Route.Api_Problems :/ (Nothing, mempty))
          formData
        let saveResponse :: R.Event t (Maybe Problem.Problem) = R.decodeText <$> response
        let errorResponse :: R.Event t (Maybe Error.Error) = R.decodeText <$> response
        errorMessage :: R.Dynamic t Text <- R.holdDyn ""
          $ maybe "Something went wrong" Error.message <$> errorResponse

        return
          ( figures
          , randomizeVariablesAction
          , resetVariablesAction
          , showAnswerAction
          , showSolutionAction
          , outputOption
          )

    outputOptionsPane editorContent figures = do
      R.elClass "div" "py-3 border-b border-brand-light-gray" $ mdo
        (randomizeVariablesAction, resetVariablesAction) <- R.elClass "div" "flex gap-2 mb-2" $ do
          randomizeVariables :: R.Event t () <- Button.primarySmallClass' "Randomize variables" "active:bg-blue-400"
          randomizeVariablesAction' :: R.Dynamic t (Loading.WithLoading (Maybe Compile.Response)) <- do
            Problem.Compile.performRequest randomizeVariables $ Compile.Request
              <$> editorContent
              <*> R.constDyn True
              <*> outputOption
              <*> figures
          resetVariables :: R.Event t () <- Button.primarySmallClass' "Reset variables" "active:bg-blue-400"
          resetVariablesAction' :: R.Dynamic t (Loading.WithLoading (Maybe Compile.Response)) <- do
            Problem.Compile.performRequest resetVariables $ Compile.Request
              <$> editorContent
              <*> R.constDyn False
              <*> outputOption
              <*> figures
          return (randomizeVariablesAction', resetVariablesAction')
        (showAnswerAction, showSolutionAction, outputOption) <- R.elClass "div" "flex gap-4" $ do
          R.elClass "p" "font-medium text-brand-primary"
            $ R.text "Show problem with:"
          R.elClass "div" "flex flex-col" $ do
            showAnswer :: R.Dynamic t Bool <- Input.checkboxClass
              "cursor-pointer mr-2 checkbox-brand-primary"
              "font-medium text-brand-primary cursor-pointer"
              "Answer"
            showAnswerAction' :: R.Dynamic t (Loading.WithLoading (Maybe Compile.Response)) <- do
              Problem.Compile.performRequest (R.updated $ const () <$> showAnswer) $ Compile.Request
                <$> editorContent
                <*> R.constDyn False
                <*> outputOption
                <*> figures
            showSolution :: R.Dynamic t Bool <- Input.checkboxClass
              "cursor-pointer mr-2 checkbox-brand-primary"
              "font-medium text-brand-primary cursor-pointer"
              "Solution"
            showSolutionAction' :: R.Dynamic t (Loading.WithLoading (Maybe Compile.Response)) <- do
              Problem.Compile.performRequest (R.updated $ const () <$> showSolution) $ Compile.Request
                <$> editorContent
                <*> R.constDyn False
                <*> outputOption
                <*> figures
            let outputOption' :: R.Dynamic t Compile.OutputOption =
                  (\showAnswer' showSolution' ->
                     case (showAnswer', showSolution') of
                       (False, False) -> Compile.QuestionOnly
                       (False, True) -> Compile.WithSolution
                       (True, False) -> Compile.WithAnswer
                       (True, True) -> Compile.WithSolutionAndAnswer
                  ) <$> showAnswer <*> showSolution
            return (showAnswerAction', showSolutionAction', outputOption')
        return (randomizeVariablesAction, resetVariablesAction, showAnswerAction, showSolutionAction, outputOption)

    mainPane preloadedProblem figures latestResponse anyLoading outputOption = do
      R.elClass "div" "pl-2 flex-1 h-full flex flex-col" $ mdo
        (uploadPrb, compileButtonAction, errorsToggle) <-
          upperPane editorContent figures outputOption latestResponse anyLoading
        let contentFromPreloadedProblem :: R.Event t Text = fromMaybe "" . fmap Problem.content
              <$> R.updated preloadedProblem
        let setContentValue = R.leftmost [contentFromPreloadedProblem, uploadPrb]
        editorContent <- R.elClass "div" "h-full flex" $ do
          editorContent' :: R.Dynamic t Text <- R.elClass "div" "flex-1" $ Editor.widget setContentValue
          R.elClass "div" "flex-1" $ PdfViewer.widget latestResponse anyLoading errorsToggle
          return editorContent'
        return (editorContent, compileButtonAction)

    upperPane editorContent figures outputOption latestResponse anyLoading = do
      R.elClass "div" "bg-brand-light-gray p-1 flex justify-between" $ do
        uploadPrb <- do
          R.elClass "div" "flex-1 flex justify-center" $ do
            R.elClass "span" "mr-auto flex gap-2 items-center" $ mdo
              uploadPrb :: R.Event t Text <- UploadPrb.widget
              DownloadPrb.widget prbName editorContent
              R.elClass "p" "ml-2" $ R.text "Filename"
              prbName :: R.Dynamic t Text <- fmap R.value $ R.inputElement
                $ R.def & R.inputElementConfig_elementConfig . R.elementConfig_initialAttributes .~
                ("type" =: "text" <> "class" =: "pl-1")
                & R.inputElementConfig_initialValue .~ "untitled"
              return uploadPrb
        compileButtonAction :: R.Dynamic t (Loading.WithLoading (Maybe Compile.Response)) <- do
          R.elClass "div" "flex-1 flex justify-center" $ do
            Problem.Compile.widget $ Compile.Request
              <$> editorContent
              <*> R.constDyn False
              <*> outputOption
              <*> figures
        errorsToggle :: R.Dynamic t Bool <- R.elClass "div" "flex-1 flex justify-center ml-auto" $ do
          R.elClass "span" "ml-auto" $ ErrorsToggle.widget latestResponse (R.updated anyLoading)
        return (uploadPrb, compileButtonAction, errorsToggle)
