{-# LANGUAGE PackageImports #-}
module Problem.Edit
  ( widget
  ) where

import Common.Lib.Prelude
import qualified Frontend.Lib.Util as Util

import qualified Control.Monad.IO.Class as IO
import qualified Data.Time.Clock as Time
import qualified Data.CaseInsensitive as CI
import qualified Data.Map as Map
import qualified Data.Text as T
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
import qualified Common.Api.ProblemStatus as ProblemStatus
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

-- Draft saving state
data SavingState a = BeforeSave | Saving | Saved | SaveError a
  deriving (Eq)

data EditContext = EditContext
  { ctxProblemId :: Maybe Integer
  , ctxAuthorId :: Integer
  , ctxSummary :: Text
  , ctxContents :: Text
  , ctxTopicId :: Integer
  , ctxFigures :: [FormFile.FormFile]
  }

autosaveInterval :: Time.NominalDiffTime
autosaveInterval = 3

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
     , R.MonadSample t (R.Performable m)
     )
  => Maybe Integer
  -> m ()
widget preloadedProblemId = mdo
  ( figures
    , randomizeVariablesAction
    , resetVariablesAction
    , showAnswerAction
    , showSolutionAction
    , outputOption
    ) <- leftPane editorContents
  ( editorContents
    , compileButtonAction
    ) <- mainPane figures
         ((Problem.Compile.response . Loading.action) <$> currentResponse)
         (Loading.loading <$> currentResponse)
         outputOption

  let actions =
        [ compileButtonAction
        , randomizeVariablesAction
        , resetVariablesAction
        , showAnswerAction
        , showSolutionAction
        ] :: [R.Dynamic t (Loading.WithLoading Problem.Compile.Response)]

  currentResponse :: R.Dynamic t (Loading.WithLoading Problem.Compile.Response) <- do
    -- The current response should have the latest request time
    let switchToLatest = \new old -> do
          let tNew = Problem.Compile.reqTime . Loading.action $ new
          let tOld = Problem.Compile.reqTime . Loading.action $ old
          if tNew >= tOld
            then Just new
            else Nothing
    t <- IO.liftIO Time.getCurrentTime
    R.foldDynMaybe
      switchToLatest
      (Loading.WithLoading (Problem.Compile.Response t Nothing) False)
      (R.leftmost . map R.updated $ actions)

  return ()

  where
    getPreloadedProblem :: m (R.Dynamic t (Maybe Problem.Problem))
    getPreloadedProblem = do
      response :: R.Event t (Maybe Problem.Problem) <- case preloadedProblemId of
        Nothing -> return R.never
        Just pid -> do
          Util.getOnload
            $ Route.apiHref $ Route.Api_Problems :/ (Just pid, mempty)
      R.holdDyn Nothing response

    fetchFigures
      ::  R.Dynamic t (Maybe Problem.Problem)
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

    leftPane editorContents = do
      R.elClass "div" "w-96 flex-none flex flex-col pr-2 border-r border-brand-light-gray" $ mdo
        when (isJust preloadedProblemId) $ do
          R.elClass "div" "pb-3 border-b border-brand-light-gray" $ do
            R.elClass "p" "font-medium mb-2" $ R.text "View"
            Ob.routeLink
              (Route.FrontendRoute_Problems :/
                (fromJust preloadedProblemId, Route.ProblemsRoute_View :/ ())) $ do
              R.elClass "p" "text-brand-primary font-medium hover:underline" $ do
                Button.secondarySmall "Public view of this problem"
          R.elClass "div" "pb-3" R.blank
        preloadedProblem :: R.Dynamic t (Maybe Problem.Problem) <- getPreloadedProblem
        let setTopicId :: R.Event t Integer =
              fromMaybe 0
              . fmap (Topic.id . Problem.topic)
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
        Util.dynFor savingState $ \case
          BeforeSave -> R.blank
          Saving -> R.elClass "p" "mt-2 text-brand-gray" $ R.text "Saving..."
          Saved -> R.elClass "p" "mt-2 text-brand-gray" $ do
            R.el "span" $ R.text "Saved to "
            Ob.routeLink (Route.FrontendRoute_Profile :/ ()) $ do
              R.elClass "span" "underline" $ R.text "drafts"
          SaveError e -> R.elClass "p" "mt-2 text-brand-gray" $ R.text e
        publish :: R.Event t () <- R.elClass "div" "py-3" $ do
          let isEditingPublishedProblem :: R.Dynamic t Bool = maybe True ((== ProblemStatus.Draft) . Problem.status) <$> preloadedProblem
          let buttonText :: R.Dynamic t Text = isEditingPublishedProblem <&> \case
                True -> "Publish"
                False -> "Publish changes"
          let button :: R.Dynamic t (m (R.Event t ())) = buttonText <&> \t ->
                Button.primaryClass' t "w-full active:bg-blue-400"
          R.dyn button >>= R.switchHold R.never -- flatten R.Event t (R.Event t ())
        publishing :: R.Behavior t Bool <- R.current <$> R.holdDyn False (const True <$> publish)
        let publishedTextMessage = publishResponse <&> \case
              Left err -> R.elClass "p" "text-red-500" $ R.text (Error.message err)
              Right publishedProblem -> do
                timer :: R.Event t R.TickInfo <- R.tickLossyFromPostBuildTime 0.01
                Ob.setRoute $ do
                  (Route.FrontendRoute_Problems :/
                   (Problem.id publishedProblem, Route.ProblemsRoute_View :/ ())) <$ timer
        let spinner = R.ffor publish $ \_ -> do
              R.elAttr "img" ("src" =: Ob.static @"small_spinner.svg" <> "width" =: "30" <> "alt" =: "loading") $ R.blank
        publishedMessage <- R.holdDyn R.blank $ R.leftmost [spinner, R.updated publishedTextMessage]
        R.dyn_ publishedMessage

        let f = \preloadedProblemId' savedProblem' -> case (preloadedProblemId', savedProblem') of
              (Just pid, _) -> Just pid
              (Nothing, Left _) -> Nothing
              (Nothing, Right p) -> Just $ Problem.id p
        let problemId :: R.Dynamic t (Maybe Integer) = f <$> R.constDyn preloadedProblemId <*> savedProblem

        userId :: R.Dynamic t Integer <-
          pure
          . R.constDyn
          . fromMaybe 0
          . fmap User.id
          =<< Util.getCurrentUser
        let ctx :: R.Dynamic t EditContext = EditContext
              <$> problemId
              -- <*> _
              <*> userId
              <*> summary
              <*> editorContents
              <*> selectedTopicId
              <*> figures

        publishResponse <- saveProblem
          publish
          ProblemStatus.Published
          ctx

        (savedProblem, savingState) <- autosaveProblem
          (andM
           [ not <$> publishing
           , maybe True ((== ProblemStatus.Draft) . Problem.status) <$> R.current preloadedProblem
           ])
          ctx

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
          randomizeVariablesAction' :: R.Dynamic t (Loading.WithLoading Problem.Compile.Response) <- do
            r <- Problem.Compile.mkRequest randomizeVariables
              editorContents
              (R.constDyn Problem.Compile.Randomize)
              outputOption
              figures
            Problem.Compile.performRequest r
          resetVariables :: R.Event t () <- Button.primarySmallClass' "Reset variables" "active:bg-blue-400"
          resetVariablesAction' :: R.Dynamic t (Loading.WithLoading Problem.Compile.Response) <- do
            r <- Problem.Compile.mkRequest resetVariables
              editorContents
              (R.constDyn Problem.Compile.Reset)
              outputOption
              figures
            Problem.Compile.performRequest r
          return (randomizeVariablesAction', resetVariablesAction')
        (showAnswerAction, showSolutionAction, outputOption) <- R.elClass "div" "flex gap-4" $ do
          R.elClass "p" "font-medium text-brand-primary"
            $ R.text "Show problem with:"
          R.elClass "div" "flex flex-col" $ do
            showAnswer :: R.Dynamic t Bool <- Input.checkboxClass
              "cursor-pointer mr-2 checkbox-brand-primary"
              "font-medium text-brand-primary cursor-pointer"
              "Answer"
            showAnswerAction' :: R.Dynamic t (Loading.WithLoading Problem.Compile.Response) <- do
              r <- Problem.Compile.mkRequest (R.updated $ const () <$> showAnswer)
                editorContents
                (R.constDyn Problem.Compile.NoChange)
                outputOption
                figures
              Problem.Compile.performRequest r
            showSolution :: R.Dynamic t Bool <- Input.checkboxClass
              "cursor-pointer mr-2 checkbox-brand-primary"
              "font-medium text-brand-primary cursor-pointer"
              "Solution"
            showSolutionAction' :: R.Dynamic t (Loading.WithLoading Problem.Compile.Response) <- do
              r <- Problem.Compile.mkRequest (R.updated $ const () <$> showSolution)
                editorContents
                (R.constDyn Problem.Compile.NoChange)
                outputOption
                figures
              Problem.Compile.performRequest r
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

    mainPane figures latestResponse anyLoading outputOption = do
      R.elClass "div" "pl-2 flex-1 h-full flex flex-col" $ mdo
        (uploadPrb, compileButtonAction, errorsToggle) <-
          upperPane editorContents figures outputOption latestResponse anyLoading
        preloadedProblem :: R.Dynamic t (Maybe Problem.Problem) <- getPreloadedProblem
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
        compileButtonAction :: R.Dynamic t (Loading.WithLoading Problem.Compile.Response) <- do
          R.elClass "div" "flex-1 flex justify-center" $ do
            compileButton <- Problem.Compile.widget
            r <- Problem.Compile.mkRequest compileButton
              editorContents
              (R.constDyn Problem.Compile.NoChange)
              outputOption
              figures
            Problem.Compile.performRequest r
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

autosaveProblem
  :: forall t m.
     ( R.DomBuilder t m
     , R.MonadHold t m
     , JS.MonadJSM (R.Performable m)
     , R.HasJSContext (R.Performable m)
     , R.PerformEvent t m
     , R.TriggerEvent t m
     , R.PostBuild t m
     , MonadFix m
     )
  => R.Behavior t Bool -- ^ Enable/disable
  -> R.Dynamic t EditContext
  -> m (R.Dynamic t (Either Error.Error Problem.Problem), R.Dynamic t (SavingState Text)) -- ^ (Problem, saving state)
autosaveProblem enable ctx = do
  autosaveTimerGreedy <- R.tickLossyFromPostBuildTime autosaveInterval
  let autosaveTimer = flip R.gate autosaveTimerGreedy $ andM
        [ enable
        , not . T.null <$> R.current (ctxContents <$> ctx)
        , not . T.null <$> R.current (ctxSummary <$> ctx)
        ]
  savedProblem <- saveProblem
    autosaveTimer
    ProblemStatus.Draft
    ctx
  savingState :: R.Dynamic t (SavingState Text) <- R.holdDyn BeforeSave $ R.leftmost
    [ const Saving <$> autosaveTimer
    , R.updated savedProblem <&> \case
        Left e -> SaveError $ Error.message e
        Right _ -> Saved
    ]
  return (savedProblem, savingState)

saveProblem
  :: forall t m a.
     ( R.DomBuilder t m
     , R.MonadHold t m
     , JS.MonadJSM (R.Performable m)
     , R.HasJSContext (R.Performable m)
     , R.PerformEvent t m
     , R.TriggerEvent t m
     )
  => R.Event t a -- ^ Trigger event
  -> ProblemStatus.Status -- ^ Problem status
  -> R.Dynamic t EditContext
  -> m (R.Dynamic t (Either Error.Error Problem.Problem)) -- ^ Response
saveProblem trg problemStatus ctx = do
  let formData :: R.Event t (Map Text (R'.FormValue GHCJS.DOM.Types.File)) =
        R.ffor (R.tagPromptlyDyn ctx trg) $ \ctx' -> do
        let formDataParams :: Map Problem.RequestParam (R'.FormValue GHCJS.DOM.Types.File) =
              ( Problem.ParamSummary =: R'.FormValue_Text (ctxSummary ctx')
                <> Problem.ParamContents =: R'.FormValue_Text (ctxContents ctx')
                <> Problem.ParamTopicId =: R'.FormValue_Text (cs . show . ctxTopicId $ ctx')
                <> Problem.ParamAuthorId =: R'.FormValue_Text (cs . show . ctxAuthorId $ ctx')
                <> Problem.ParamStatus =: R'.FormValue_Text (cs . show $ problemStatus)
                <> Problem.ParamFigures =: R'.FormValue_List (map Util.formFile . ctxFigures $ ctx')
              )
              <>
              ( maybe
                mempty
                (\id' -> Problem.ParamProblemId =: R'.FormValue_Text (cs . show $ id'))
                (ctxProblemId ctx')
              )
        Map.mapKeys (cs . show) formDataParams
  rawResponse :: R.Event t Text <- Util.postForm
    (Route.apiHref $ Route.Api_Problems :/ (Nothing, mempty))
    formData
  publishResponse :: R.Dynamic t (Maybe Problem.Problem) <- R.holdDyn Nothing $ R.decodeText <$> rawResponse
  errorResponse :: R.Dynamic t (Maybe Error.Error) <- R.holdDyn Nothing $ R.decodeText <$> rawResponse

  return $ R.zipDyn publishResponse errorResponse <&> \case
    (Nothing, Nothing) -> Left $ Error.mk "Something went wrong"
    (Nothing, Just err) -> Left err
    (Just pr, _) -> Right pr
