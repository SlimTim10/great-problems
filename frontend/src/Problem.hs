{-# LANGUAGE PackageImports #-}
module Problem
  ( widget
  ) where

import qualified Language.Javascript.JSaddle as JS
import qualified Data.Aeson as JSON
import qualified "jsaddle-dom" GHCJS.DOM.Document as DOM
import qualified Obelisk.Route.Frontend as Ob
import qualified Reflex.Dom.Core as R

import qualified Common.Route as Route
import qualified Common.File
import qualified Common.Compile
import qualified Common.Api.NewProblem as NewProblem
import qualified Common.Api.Problem as Problem
import qualified Common.Api.User as User
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
  => m ()
widget = mdo
  ( figures
    , randomizeVariablesAction
    , resetVariablesAction
    , showAnswerAction
    , showSolutionAction
    , outputOption
    ) <- leftPane editorContent prbName
    
  ( editorContent
    , prbName
    , compileButtonAction
    ) <- mainPane
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
  latestResponse :: R.Dynamic t (Maybe Common.Compile.CompileResponse) <- R.holdDyn Nothing
    $ R.leftmost . map (R.updated . fmap action)
    $ actions

  -- If any response is still loading, we want to show the loading spinner
  let loadings :: [R.Event t Bool] = map (R.updated . fmap loading) $ actions
  anyLoading :: R.Dynamic t Bool <- do
    anyLoading' :: R.Event t Bool <- R.performEvent $ R.ffor (R.leftmost loadings) $ \_ -> do
      loadingSamples <- mapM (R.sample . R.current) . map (fmap loading) $ actions
      return $ any (== True) loadingSamples
    R.holdDyn False anyLoading'

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
          , showAnswerAction
          , showSolutionAction
          , outputOption
          ) <- outputOptionsPane editorContent prbName figures
        figures :: R.Dynamic t [Common.File.FileWithName] <- R.elClass "div" "py-3 border-b border-brand-light-gray"
          $ Figures.widget
        publish :: R.Event t () <- R.elClass "div" "py-3" $ do
          Button.primaryClass' "Save & Publish" "w-full active:bg-blue-400"
        publishMessage <- R.holdDyn R.blank $ \case
          Nothing -> do
            R.elClass "p" "text-red-500" $ R.text "Something went wrong"
          Just publishedProblem -> do
            R.el "p" $ R.text "Published!"
            Ob.routeLink
              (Route.FrontendRoute_Problems :/
                (Problem.id publishedProblem, Route.ProblemsRoute_View :/ ())) $ do
              R.elClass "p" "text-brand-primary font-medium hover:underline" $ do
                R.text "Click here to view your published problem"
          <$> publishResponse
        R.dyn_ publishMessage

        userId :: R.Dynamic t Integer <-
          pure
          . R.constDyn
          . fromMaybe 0
          . fmap User.id
          =<< Util.getCurrentUser
        let newProblem :: R.Dynamic t NewProblem.NewProblem = NewProblem.NewProblem
              <$> summary
              <*> editorContent
              <*> selectedTopicId
              <*> userId
        let ev :: R.Event t NewProblem.NewProblem = R.tagPromptlyDyn newProblem publish
        r <- R.performRequestAsync $ newProblemRequest <$> ev
        let publishResponse :: R.Event t (Maybe Problem.Problem) = R.decodeXhrResponse <$> r
        
        return
          ( figures
          , randomizeVariablesAction
          , resetVariablesAction
          , showAnswerAction
          , showSolutionAction
          , outputOption
          )

    newProblemRequest :: JSON.ToJSON a => a -> R.XhrRequest Text
    newProblemRequest body = R.postJson url body
      where url = Route.apiHref (Route.Api_Problems :/ (Nothing, mempty))

    outputOptionsPane editorContent prbName figures = do
      R.elClass "div" "py-3 border-b border-brand-light-gray" $ mdo
        (randomizeVariablesAction, resetVariablesAction) <- R.elClass "div" "flex gap-2 mb-2" $ do
          randomizeVariables :: R.Event t () <- Button.primarySmallClass' "Randomize variables" "active:bg-blue-400"
          randomizeVariablesAction' :: R.Dynamic t (WithLoading (Maybe Common.Compile.CompileResponse)) <- do
            (fmap . fmap $ uncurry WithLoading) $ do
              Compile.performRequest randomizeVariables $ Common.Compile.CompileRequest
                <$> editorContent
                <*> prbName
                <*> R.constDyn True
                <*> outputOption
                <*> figures
          resetVariables :: R.Event t () <- Button.primarySmallClass' "Reset variables" "active:bg-blue-400"
          resetVariablesAction' :: R.Dynamic t (WithLoading (Maybe Common.Compile.CompileResponse)) <- do
            (fmap . fmap $ uncurry WithLoading) $ do
              Compile.performRequest resetVariables $ Common.Compile.CompileRequest
                <$> editorContent
                <*> prbName
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
            showAnswerAction' :: R.Dynamic t (WithLoading (Maybe Common.Compile.CompileResponse)) <- do
              (fmap . fmap $ uncurry WithLoading) $ do
              Compile.performRequest (R.updated $ const () <$> showAnswer) $ Common.Compile.CompileRequest
                <$> editorContent
                <*> prbName
                <*> R.constDyn False
                <*> outputOption
                <*> figures
            showSolution :: R.Dynamic t Bool <- Input.checkboxClass
              "cursor-pointer mr-2 checkbox-brand-primary"
              "font-medium text-brand-primary cursor-pointer"
              "Solution"
            showSolutionAction' :: R.Dynamic t (WithLoading (Maybe Common.Compile.CompileResponse)) <- do
              (fmap . fmap $ uncurry WithLoading) $ do
              Compile.performRequest (R.updated $ const () <$> showSolution) $ Common.Compile.CompileRequest
                <$> editorContent
                <*> prbName
                <*> R.constDyn False
                <*> outputOption
                <*> figures
            let outputOption' :: R.Dynamic t Common.Compile.OutputOption =
                  (\showAnswer' showSolution' ->
                     case (showAnswer', showSolution') of
                       (False, False) -> Common.Compile.QuestionOnly
                       (False, True) -> Common.Compile.WithSolution
                       (True, False) -> Common.Compile.WithAnswer
                       (True, True) -> Common.Compile.WithSolutionAndAnswer
                  ) <$> showAnswer <*> showSolution
            return (showAnswerAction', showSolutionAction', outputOption')
        return (randomizeVariablesAction, resetVariablesAction, showAnswerAction, showSolutionAction, outputOption)

    mainPane figures latestResponse anyLoading outputOption = do
      R.elClass "div" "pl-2 flex-1 h-full flex flex-col" $ mdo
        (uploadPrb, prbName, compileButtonAction, errorsToggle) <-
          upperPane editorContent figures outputOption latestResponse anyLoading
        editorContent <- R.elClass "div" "h-full flex" $ do
          editorContent' :: R.Dynamic t Text <- R.elClass "div" "flex-1" $ Editor.widget uploadPrb
          R.elClass "div" "flex-1" $ PdfViewer.widget latestResponse anyLoading errorsToggle
          return editorContent'
        return (editorContent, prbName, compileButtonAction)

    upperPane editorContent figures outputOption latestResponse anyLoading = do
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
                <*> outputOption
                <*> figures
        errorsToggle :: R.Dynamic t Bool <- R.elClass "div" "flex-1 flex justify-center ml-auto" $ do
          R.elClass "span" "ml-auto" $ ErrorsToggle.widget latestResponse (R.updated anyLoading)
        return (uploadPrb, prbName, compileButtonAction, errorsToggle)
