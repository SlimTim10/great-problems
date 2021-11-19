{-# LANGUAGE PackageImports #-}
module Problem.View
  ( widget
  ) where

import Common.Lib.Prelude
import qualified Frontend.Lib.Util as Util

import qualified Language.Javascript.JSaddle as JS
import qualified "ghcjs-dom" GHCJS.DOM.Document as DOM
import qualified Data.CaseInsensitive as CI
import qualified Obelisk.Route.Frontend as Ob
import qualified Reflex.Dom.Core as R

import qualified Common.Route as Route
import qualified Common.Api.Compile as Compile
import qualified Common.Api.Problem as Problem
import qualified Common.Api.User as User
import qualified Common.Api.Topic as Topic
import qualified Problem.PdfViewer as PdfViewer
import qualified Widget.Button as Button
import qualified Widget.Input as Input
import qualified Problem.Loading as Loading
import qualified Problem.Compile

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
     , Ob.SetRoute t (Ob.R Route.FrontendRoute) m
     , Ob.RouteToUrl (Ob.R Route.FrontendRoute) m
     , R.Prerender js t m
     , R.HasDocument m
     , DOM.IsDocument (R.RawDocument (R.DomBuilderSpace m))
     )
  => Integer
  -> m ()
widget problemId = mdo
  topicPath
  
  ( randomizeVariablesAction
    , resetVariablesAction
    , showAnswerAction
    , showSolutionAction
    ) <- fullView latestResponse anyLoading
    
  onload :: R.Event t () <- R.getPostBuild
  onloadAction :: R.Dynamic t (Loading.WithLoading (Maybe Compile.Response)) <- do
    Problem.Compile.performRequestWithId onload problemId $ Problem.Compile.Request
      <$> R.constDyn ""
      <*> R.constDyn Problem.Compile.NoChange
      <*> R.constDyn Compile.QuestionOnly
      <*> R.constDyn []

  let actions =
        [ onloadAction
        , randomizeVariablesAction
        , resetVariablesAction
        , showAnswerAction
        , showSolutionAction
        ]

  latestResponse :: R.Dynamic t (Maybe Compile.Response) <- R.holdDyn Nothing
    $ Loading.latestAction actions

  anyLoading :: R.Dynamic t Bool <- Loading.anyLoading actions

  return ()
  where
    getUserId :: m (R.Dynamic t Integer)
    getUserId = pure . R.constDyn . fromMaybe 0 . fmap User.id =<< Util.getCurrentUser

    getProblem :: m (R.Dynamic t (Maybe Problem.Problem))
    getProblem = do
      r :: R.Event t (Maybe Problem.Problem) <- Util.getOnload
        $ Route.apiHref $ Route.Api_Problems :/
        ( Just problemId, Problem.getParamsToRouteQuery
          $ Problem.GetParams
          { Problem.gpExpand = Just ["author", "topic"]
          , Problem.gpInclude = Just Problem.TopicPath
          , Problem.gpTopic = Nothing
          }
        )
      R.holdDyn Nothing r

    topicPath = do
      R.elClass "div" "bg-brand-light-gray flex py-2 pl-2" $ do
        problem <- getProblem
        Util.dynFor problem $ \case
          Nothing -> R.blank
          Just p -> do
            R.elClass "div" "flex" $ do
              let topics = fromMaybe [] $ Problem.topicPath p
              forM_ (zip [0..] topics) $ \(n :: Integer, Topic.Topic tid name _) -> do
                unless (n == 0) $ do
                  R.elClass "p" "text-brand-gray mx-1" $ R.text ">"
                Ob.routeLink
                  (Route.FrontendRoute_Topics :/ (tid, Route.TopicsRoute_Problems :/ ())) $ do
                  R.elClass "p" "hover:underline text-brand-primary" $ R.text name

    fullView latestResponse anyLoading = do
      R.elClass "div" "flex-1 mx-2 flex justify-center" $ do
        R.elClass "div" "w-brand-screen-lg flex" $ do
          ctx <- leftPane
          R.elClass "div" "pl-2 flex-1 h-full flex flex-col" $ do
            R.elClass "div" "flex-1" $ do
              PdfViewer.widget latestResponse anyLoading (R.constDyn False)
          return ctx

    leftPane = do
      R.elClass "div" "w-96 flex-none flex flex-col gap-2" $ mdo
        section problemDetails
        section $ do
          R.elClass "div" "flex gap-6" $ do
            R.elClass "p" "text-brand-sm text-brand-primary font-medium" $ do
              R.text "SHARE"
            R.elClass "p" "text-brand-sm text-brand-primary font-medium" $ do
              R.text "SAVE"
        ctx <- problemOptions
        return ctx
      where
        section body = R.elClass "div" "pt-1 pb-3 border-b border-brand-light-gray" $ body

    problemOptions = mdo
      (randomizeVariablesAction, resetVariablesAction) <- do
        R.elClass "div" "py-3 flex gap-2" $ mdo
          randomizeVariables :: R.Event t () <- Button.primarySmallClass'
            "Randomize variables"
            "active:bg-blue-400"
          randomizeVariablesAction :: R.Dynamic t (Loading.WithLoading (Maybe Compile.Response)) <- do
            Problem.Compile.performRequestWithId randomizeVariables problemId $ Problem.Compile.Request
              <$> R.constDyn ""
              <*> R.constDyn Problem.Compile.Randomize
              <*> outputOption
              <*> R.constDyn []
          resetVariables :: R.Event t () <- Button.primarySmallClass'
            "Reset variables"
            "active:bg-blue-400"
          resetVariablesAction :: R.Dynamic t (Loading.WithLoading (Maybe Compile.Response)) <- do
            Problem.Compile.performRequestWithId resetVariables problemId $ Problem.Compile.Request
              <$> R.constDyn ""
              <*> R.constDyn Problem.Compile.Reset
              <*> outputOption
              <*> R.constDyn []
          return (randomizeVariablesAction, resetVariablesAction)
      (showAnswerAction, showSolutionAction, outputOption) <- do
        R.elClass "div" "flex gap-4" $ do
          R.elClass "p" "font-medium text-brand-primary"
            $ R.text "Show problem with:"
          R.elClass "div" "flex flex-col" $ do
            showAnswer :: R.Dynamic t Bool <- Input.checkboxClass
              "cursor-pointer mr-2 checkbox-brand-primary"
              "font-medium text-brand-primary cursor-pointer"
              "Answer"
            showAnswerAction' :: R.Dynamic t (Loading.WithLoading (Maybe Compile.Response)) <- do
              Problem.Compile.performRequestWithId (R.updated $ const () <$> showAnswer) problemId $ Problem.Compile.Request
                <$> R.constDyn ""
                <*> R.constDyn Problem.Compile.NoChange
                <*> outputOption
                <*> R.constDyn []
            showSolution :: R.Dynamic t Bool <- Input.checkboxClass
              "cursor-pointer mr-2 checkbox-brand-primary"
              "font-medium text-brand-primary cursor-pointer"
              "Solution"
            showSolutionAction' :: R.Dynamic t (Loading.WithLoading (Maybe Compile.Response)) <- do
              Problem.Compile.performRequestWithId (R.updated $ const () <$> showSolution) problemId $ Problem.Compile.Request
                <$> R.constDyn ""
                <*> R.constDyn Problem.Compile.NoChange
                <*> outputOption
                <*> R.constDyn []
            let outputOption' :: R.Dynamic t Compile.OutputOption =
                  (\showAnswer' showSolution' ->
                     case (showAnswer', showSolution') of
                       (False, False) -> Compile.QuestionOnly
                       (False, True) -> Compile.WithSolution
                       (True, False) -> Compile.WithAnswer
                       (True, True) -> Compile.WithSolutionAndAnswer
                  ) <$> showAnswer <*> showSolution
            return (showAnswerAction', showSolutionAction', outputOption')
      return
        ( randomizeVariablesAction
        , resetVariablesAction
        , showAnswerAction
        , showSolutionAction
        )

    problemDetails = do
      R.elClass "div" "flex flex-col gap-1" $ do
        let problemDetails' = \p -> do
              R.elClass "p" "text-brand-sm text-brand-gray" $ do
                R.text $ "#" <> (cs . show . Problem.id $ p)
              R.elClass "p" "font-medium" $ do
                R.text $ Problem.summary p
              R.elClass "p" "text-brand-sm text-brand-gray" $ do
                R.text $ "Last updated at " <> (cs . show $ Problem.updatedAt p)
              R.elClass "div" "flex gap-1" $ do
                R.elClass "p" "text-brand-sm text-brand-gray" $ do
                  R.text $ "by"
                R.elClass "p" "text-brand-sm text-brand-gray font-bold" $ do
                  R.text $ either (const "") (CI.original . User.fullName) (Problem.author p)
        problem <- getProblem
        R.dyn_ $ maybe R.blank problemDetails' <$> problem
        editProblem


    editProblem = do
      let showEditLink uid = \case
            Nothing -> R.blank
            Just p -> do
              let authorId = either id User.id (Problem.author p)
              when (authorId == uid) $ do
                Ob.routeLink
                  (Route.FrontendRoute_Problems :/
                   (problemId, Route.ProblemsRoute_Edit :/ ())) $ do
                  R.elClass "div" "my-1" $ Button.secondarySmall "Edit this problem"
      userId <- getUserId
      problem <- getProblem
      R.dyn_ $ showEditLink <$> userId <*> problem
