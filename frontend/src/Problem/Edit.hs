{-# LANGUAGE PackageImports #-}
module Problem.Edit
  ( widget
  ) where

import Common.Lib.Prelude
import qualified Frontend.Lib.Util as Util

import qualified Data.CaseInsensitive as CI
import qualified Data.Map as Map
import qualified Language.Javascript.JSaddle as JS
import qualified "ghcjs-dom" GHCJS.DOM.Document as DOM
import qualified GHCJS.DOM.Types
import qualified Obelisk.Generated.Static as Ob
import qualified Obelisk.Route.Frontend as Ob
import qualified Reflex.Dom.Core as R
-- Import patch
import qualified MyReflex.Dom.Xhr.FormData as R'

import qualified Common.Route as Route
import qualified Common.Api.Compile as Compile
import qualified Common.Api.Error as Error
import qualified Common.Api.Problem as Problem
import qualified Common.Api.Figure as Figure
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
import qualified Problem.FormFile as FormFile
import qualified Widget.Button as Button
import qualified Widget.Input as Input

-- Update or publish new problem
data RequestSave = RequestSave
  { rsProblemId :: Maybe Integer
  , rsSummary :: Text
  , rsContents :: Text
  , rsTopicId :: Integer
  , rsAuthorId :: Integer
  , rsFigures :: [FormFile.FormFile]
  }

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
    ) <- leftPane preloadedProblem editorContents
    
  ( editorContents
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
    leftPane preloadedProblem editorContents = do
      R.elClass "div" "w-96 flex-none flex flex-col pr-2 border-r border-brand-light-gray" $ mdo
        when (isJust problemId) $ do
          R.elClass "div" "pb-3 border-b border-brand-light-gray" $ do
            R.elClass "p" "font-medium mb-2" $ R.text "View"
            Ob.routeLink
              (Route.FrontendRoute_Problems :/
                (fromJust problemId, Route.ProblemsRoute_View :/ ())) $ do
              R.elClass "p" "text-brand-primary font-medium hover:underline" $ do
                Button.secondarySmall "Public view of this problem"
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
          ) <- outputOptionsPane editorContents figures
        figures :: R.Dynamic t [FormFile.FormFile] <- R.elClass "div" "py-3 border-b border-brand-light-gray"
          $ Figures.widget =<< fetchFigures preloadedProblem
        publish :: R.Event t () <- R.elClass "div" "py-3" $ do
          Button.primaryClass' "Save & Publish" "w-full active:bg-blue-400"
        let isEditing = isJust problemId
        finishMessage <- R.holdDyn R.blank $ saveResponse <&> \case
          Nothing -> R.elClass "p" "text-red-500" $ R.dynText errorMessage
          Just savedProblem -> case isEditing of
            True -> R.elClass "p" "text-green-600" $ R.text "Saved!"
            False -> do
              timer :: R.Event t R.TickInfo <- R.tickLossyFromPostBuildTime 0.01
              Ob.setRoute $ do
                (Route.FrontendRoute_Problems :/
                 (Problem.id savedProblem, Route.ProblemsRoute_View :/ ())) <$ timer
        let spinner = R.ffor publish $ \_ -> do
              R.elAttr "img" ("src" =: Ob.static @"small_spinner.svg" <> "width" =: "30" <> "alt" =: "loading") $ R.blank
        message <- R.holdDyn R.blank $ R.leftmost [spinner, R.updated finishMessage]
        R.dyn_ message

        userId :: R.Dynamic t Integer <-
          pure
          . R.constDyn
          . fromMaybe 0
          . fmap User.id
          =<< Util.getCurrentUser

        let saveProblemRequest :: R.Dynamic t RequestSave = case problemId of
              Nothing -> RequestSave
                <$> R.constDyn Nothing
                <*> summary
                <*> editorContents
                <*> selectedTopicId
                <*> userId
                <*> figures
              Just pid -> RequestSave
                <$> R.constDyn (Just pid)
                <*> summary
                <*> editorContents
                <*> selectedTopicId
                <*> userId
                <*> figures

        let formData :: R.Event t (Map Text (R'.FormValue GHCJS.DOM.Types.File)) = R.ffor (R.tagPromptlyDyn saveProblemRequest publish) $ \req -> do
              let formDataParams :: Map Problem.RequestParam (R'.FormValue GHCJS.DOM.Types.File) =
                    ( Problem.ParamSummary =: R'.FormValue_Text (rsSummary req)
                      <> Problem.ParamContents =: R'.FormValue_Text (rsContents req)
                      <> Problem.ParamTopicId =: R'.FormValue_Text
                      (cs . show . rsTopicId $ req)
                      <> Problem.ParamAuthorId =: R'.FormValue_Text
                      (cs . show . rsAuthorId $ req)
                      <> Problem.ParamFigures =: R'.FormValue_List
                      (map Util.formFile . rsFigures $ req)
                    )
                    <>
                    ( maybe
                      mempty
                      (\id' -> Problem.ParamProblemId =: R'.FormValue_Text (cs . show $ id'))
                      (rsProblemId req)
                    )
              Map.mapKeys (cs . show) formDataParams
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

    outputOptionsPane editorContents figures = do
      R.elClass "div" "py-3 border-b border-brand-light-gray" $ mdo
        (randomizeVariablesAction, resetVariablesAction) <- R.elClass "div" "flex gap-2 mb-2" $ do
          randomizeVariables :: R.Event t () <- Button.primarySmallClass' "Randomize variables" "active:bg-blue-400"
          randomizeVariablesAction' :: R.Dynamic t (Loading.WithLoading (Maybe Compile.Response)) <- do
            Problem.Compile.performRequest randomizeVariables $ Problem.Compile.Request
              <$> editorContents
              <*> R.constDyn Problem.Compile.Randomize
              <*> outputOption
              <*> figures
          resetVariables :: R.Event t () <- Button.primarySmallClass' "Reset variables" "active:bg-blue-400"
          resetVariablesAction' :: R.Dynamic t (Loading.WithLoading (Maybe Compile.Response)) <- do
            Problem.Compile.performRequest resetVariables $ Problem.Compile.Request
              <$> editorContents
              <*> R.constDyn Problem.Compile.Reset
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
              Problem.Compile.performRequest (R.updated $ const () <$> showAnswer) $ Problem.Compile.Request
                <$> editorContents
                <*> R.constDyn Problem.Compile.NoChange
                <*> outputOption
                <*> figures
            showSolution :: R.Dynamic t Bool <- Input.checkboxClass
              "cursor-pointer mr-2 checkbox-brand-primary"
              "font-medium text-brand-primary cursor-pointer"
              "Solution"
            showSolutionAction' :: R.Dynamic t (Loading.WithLoading (Maybe Compile.Response)) <- do
              Problem.Compile.performRequest (R.updated $ const () <$> showSolution) $ Problem.Compile.Request
                <$> editorContents
                <*> R.constDyn Problem.Compile.NoChange
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
          upperPane editorContents figures outputOption latestResponse anyLoading
        let contentsFromPreloadedProblem :: R.Event t Text = fromMaybe "" . fmap Problem.contents
              <$> R.updated preloadedProblem
        let setContentsValue = R.leftmost [contentsFromPreloadedProblem, uploadPrb]
        editorContents <- R.elClass "div" "h-full flex" $ do
          editorContents' :: R.Dynamic t Text <- R.elClass "div" "flex-1" $ Editor.widget setContentsValue
          R.elClass "div" "flex-1" $ PdfViewer.widget latestResponse anyLoading errorsToggle
          return editorContents'
        return (editorContents, compileButtonAction)

    upperPane editorContents figures outputOption latestResponse anyLoading = do
      R.elClass "div" "bg-brand-light-gray p-1 flex justify-between" $ do
        uploadPrb <- do
          R.elClass "div" "flex-1 flex justify-center" $ do
            R.elClass "span" "mr-auto flex gap-2 items-center" $ mdo
              uploadPrb :: R.Event t Text <- UploadPrb.widget
              DownloadPrb.widget prbName editorContents
              R.elClass "p" "ml-2" $ R.text "Filename"
              prbName :: R.Dynamic t Text <- fmap R.value $ R.inputElement
                $ R.def & R.inputElementConfig_elementConfig . R.elementConfig_initialAttributes .~
                ("type" =: "text" <> "class" =: "pl-1")
                & R.inputElementConfig_initialValue .~ "untitled"
              return uploadPrb
        compileButtonAction :: R.Dynamic t (Loading.WithLoading (Maybe Compile.Response)) <- do
          R.elClass "div" "flex-1 flex justify-center" $ do
            Problem.Compile.widget $ Problem.Compile.Request
              <$> editorContents
              <*> R.constDyn Problem.Compile.NoChange
              <*> outputOption
              <*> figures
        errorsToggle :: R.Dynamic t Bool <- R.elClass "div" "flex-1 flex justify-center ml-auto" $ do
          R.elClass "span" "ml-auto" $ ErrorsToggle.widget latestResponse (R.updated anyLoading)
        return (uploadPrb, compileButtonAction, errorsToggle)

responseToFile :: R.XhrResponse -> Maybe FormFile.FormFile
responseToFile response = do
  body <- response ^. R.xhrResponse_response
  case body of
    R.XhrResponseBody_Blob blob -> do
      let v = GHCJS.DOM.Types.unBlob blob
      let file :: GHCJS.DOM.Types.File = GHCJS.DOM.Types.pFromJSVal v
      let headers = response ^. R.xhrResponse_headers
      name <- Map.lookup (CI.mk "Filename") headers
      return $ FormFile.FormFile file name
    _ -> Nothing

fetchFigures
  :: forall t m.
     ( R.PerformEvent t m
     , JS.MonadJSM (R.Performable m)
     , R.HasJSContext (R.Performable m)
     , R.TriggerEvent t m
     , R.MonadHold t m
     )
  => R.Dynamic t (Maybe Problem.Problem)
  -> m (R.Dynamic t [FormFile.FormFile])
fetchFigures problem = do
  let urls :: R.Event t [Text] = R.ffor (R.updated problem) $ \case
        Nothing -> []
        Just p -> R.ffor (Problem.figures p) $ \x -> Route.apiHref $ Route.Api_Figures :/ (Figure.id x)
  let requests :: R.Event t [R.XhrRequest ()] = (fmap . map)
        (\x -> R.XhrRequest "GET" x $ R.def
          & R.xhrRequestConfig_responseType .~ Just R.XhrResponseType_Blob
          & R.xhrRequestConfig_responseHeaders .~ R.AllHeaders
        )
        urls
  responses :: R.Event t [R.XhrResponse] <- R.performRequestsAsync requests
  let files :: R.Event t [FormFile.FormFile] = catMaybes . map responseToFile <$> responses
  R.holdDyn [] files
