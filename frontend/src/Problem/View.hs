{-# LANGUAGE PackageImports #-}
module Problem.View
  ( widget
  ) where

import qualified Data.Aeson as JSON
import qualified Control.Monad.IO.Class as IO
import qualified Data.Time.Clock as Time
import qualified Language.Javascript.JSaddle as JS
import qualified "ghcjs-dom" GHCJS.DOM.Document as DOM
import qualified Data.CaseInsensitive as CI
import qualified Obelisk.Route.Frontend as Ob
import qualified Reflex.Dom.Core as R

import Common.Lib.Prelude
import qualified Frontend.Lib.Util as Util
import qualified Common.Route as Route
import qualified Common.Api.Compile as Compile
import qualified Common.Api.Problem as Problem
import qualified Common.Api.ProblemStatus as ProblemStatus
import qualified Common.Api.User as User
import qualified Common.Api.Topic as Topic
import qualified Common.Api.Role as Role
import qualified Common.Api.MetaSetting as MetaSetting
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
     , R.MonadSample t (R.Performable m)
     )
  => Integer
  -> m ()
widget problemId = mdo
  problem :: R.Dynamic t (Maybe Problem.Problem) <- getProblem
  let redirect = problem <&> \case
        Nothing -> R.blank
        Just problem' -> case Problem.status problem' of
          -- Can't view drafts; redirect to edit
          ProblemStatus.Draft -> do
            afterLoad :: R.Event t R.TickInfo <- R.tickLossyFromPostBuildTime 0.01
            Ob.setRoute
              $ (Route.FrontendRoute_Problems :/ (Problem.id problem', Route.ProblemsRoute_Edit :/ ())) <$ afterLoad
          _ -> R.blank
  R.dyn_ redirect
  
  topicPath
  ( randomizeVariablesAction
    , resetVariablesAction
    , showAnswerAction
    , showSolutionAction
    ) <- problemPane
         ((Problem.Compile.response . Loading.action) <$> currentResponse)
         (Loading.loading <$> currentResponse)
    
  onload :: R.Event t () <- R.getPostBuild
  onloadAction :: R.Dynamic t (Loading.WithLoading Problem.Compile.Response) <- do
    r <- Problem.Compile.mkRequest onload
      (R.constDyn "")
      (R.constDyn Problem.Compile.NoChange)
      (R.constDyn Compile.QuestionOnly)
      (R.constDyn [])
    Problem.Compile.performRequestWithId problemId r

  let actions =
        [ onloadAction
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
    getUserId :: m (R.Dynamic t Integer)
    getUserId = pure . R.constDyn . fromMaybe 0 . fmap User.id =<< Util.getCurrentUser

    getProblem :: m (R.Dynamic t (Maybe Problem.Problem))
    getProblem = do
      res :: R.Event t (Maybe Problem.Problem) <- Util.getOnload
        $ Route.apiHref $ Route.Api_Problems :/ (Just problemId, mempty)
      R.holdDyn Nothing res

    getBasicDuplicateTopicIds :: m (R.Dynamic t [Integer])
    getBasicDuplicateTopicIds = do
      res :: R.Event t (Maybe MetaSetting.MetaSetting) <- Util.getOnload
        $ Route.apiHref $ Route.Api_MetaSettings :/ (Just MetaSetting.BasicDuplicateTopicIds)
      let basicDuplicateTopicIds :: R.Event t [Integer] = res <&> \case
            Nothing -> []
            Just x -> case JSON.decode (cs . MetaSetting.value $ x) :: Maybe [Integer] of
              Nothing -> []
              Just y -> y
      R.holdDyn [] basicDuplicateTopicIds

    topicPath = do
      R.elClass "div" "bg-brand-light-gray flex py-2 pl-2" $ do
        problem <- getProblem
        Util.dynFor problem $ \case
          Nothing -> R.blank
          Just p -> do
            R.elClass "div" "flex" $ do
              let topics = Problem.topicPath p
              forM_ (zip [0..] topics) $ \(n :: Integer, Topic.Topic tid name _) -> do
                unless (n == 0) $ do
                  R.elClass "p" "text-brand-gray mx-1" $ R.text ">"
                Ob.routeLink
                  (Route.FrontendRoute_Topics :/ (tid, Route.TopicsRoute_Problems :/ ())) $ do
                  R.elClass "p" "hover:underline text-brand-primary" $ R.text name

    problemPane latestResponse anyLoading = do
      R.elClass "div" "flex-1 mx-2 flex justify-center" $ do
        R.elClass "div" "w-brand-screen-lg flex" $ do
          ctx <- infoPane
          R.elClass "div" "pl-2 flex-1 h-full flex flex-col" $ do
            R.elClass "div" "flex-1" $ do
              PdfViewer.widget latestResponse anyLoading (R.constDyn False)
          return ctx

    infoPane = do
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
          randomizeVariablesAction :: R.Dynamic t (Loading.WithLoading Problem.Compile.Response) <- do
            r <- Problem.Compile.mkRequest randomizeVariables
              (R.constDyn "")
              (R.constDyn Problem.Compile.Randomize)
              outputOption
              (R.constDyn [])
            Problem.Compile.performRequestWithId problemId r
          resetVariables :: R.Event t () <- Button.primarySmallClass'
            "Reset variables"
            "active:bg-blue-400"
          resetVariablesAction :: R.Dynamic t (Loading.WithLoading Problem.Compile.Response) <- do
            r <- Problem.Compile.mkRequest resetVariables
              (R.constDyn "")
              (R.constDyn Problem.Compile.Reset)
              outputOption
              (R.constDyn [])
            Problem.Compile.performRequestWithId problemId r
          return (randomizeVariablesAction, resetVariablesAction)
      (showAnswerAction, showSolutionAction, outputOption) <- do
        R.elClass "div" "flex gap-4" $ do
          R.elClass "p" "font-medium text-brand-primary"
            $ R.text "Show problem with:"
          R.elClass "div" "flex flex-col" $ mdo
            showAnswer :: R.Dynamic t Bool <- Input.checkboxClass
              "cursor-pointer mr-2 checkbox-brand-primary"
              "font-medium text-brand-primary cursor-pointer"
              "Answer"
            showAnswerAction :: R.Dynamic t (Loading.WithLoading Problem.Compile.Response) <- do
              r <- Problem.Compile.mkRequest (R.updated $ const () <$> showAnswer)
                (R.constDyn "")
                (R.constDyn Problem.Compile.NoChange)
                outputOption
                (R.constDyn [])
              Problem.Compile.performRequestWithId problemId r
            showSolution :: R.Dynamic t Bool <- Input.checkboxClass
              "cursor-pointer mr-2 checkbox-brand-primary"
              "font-medium text-brand-primary cursor-pointer"
              "Solution"
            showSolutionAction :: R.Dynamic t (Loading.WithLoading Problem.Compile.Response) <- do
              r <- Problem.Compile.mkRequest (R.updated $ const () <$> showSolution)
                (R.constDyn "")
                (R.constDyn Problem.Compile.NoChange)
                outputOption
                (R.constDyn [])
              Problem.Compile.performRequestWithId problemId r
            let outputOption' :: R.Dynamic t Compile.OutputOption =
                  (\showAnswer' showSolution' ->
                     case (showAnswer', showSolution') of
                       (False, False) -> Compile.QuestionOnly
                       (False, True) -> Compile.WithSolution
                       (True, False) -> Compile.WithAnswer
                       (True, True) -> Compile.WithSolutionAndAnswer
                  ) <$> showAnswer <*> showSolution
            return (showAnswerAction, showSolutionAction, outputOption')
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
                  R.text $ (CI.original . User.fullName) (Problem.author p)
        problem <- getProblem
        R.dyn_ $ maybe R.blank problemDetails' <$> problem
        editProblemButton
        duplicateProblemButton

    editProblemButton = do
      let showEditLink = \uid p -> case p of
            Nothing -> R.blank
            Just p' -> do
              let authorId = User.id (Problem.author p')
              when (authorId == uid) $ do
                Ob.routeLink
                  (Route.FrontendRoute_Problems :/
                   (problemId, Route.ProblemsRoute_Edit :/ ())) $ do
                  R.elClass "div" "my-1" $ Button.secondarySmall "Edit problem"
      userId <- getUserId
      problem <- getProblem
      R.dyn_ $ showEditLink <$> userId <*> problem

    duplicateProblemButton = do
      let duplicateLink = \p -> do
            Ob.routeLink (Route.FrontendRoute_DuplicateProblem :/ Problem.id p) $ do
              R.elClass "div" "my-1" $ Button.secondarySmall "Duplicate problem"
      let showDuplicateLink = \p -> case p of
            Nothing -> R.blank
            Just p' -> duplicateLink p'
      let showDuplicateLinkForBasicUser = \topicIds p -> case p of
            Nothing -> R.blank
            Just p' ->
              if (Topic.id . Problem.topic $ p') `elem` topicIds
              then duplicateLink p'
              else R.blank
      Util.getCurrentUser >>= \case
        Nothing -> R.blank
        Just user ->
          if
            | User.role user `elem` [Role.Contributor, Role.Moderator, Role.Administrator]
              -> do
                problem <- getProblem
                R.dyn_ $ showDuplicateLink <$> problem
            | User.role user == Role.Basic
              -> do
                problem <- getProblem
                basicDuplicateTopicIds <- getBasicDuplicateTopicIds
                R.dyn_ $ showDuplicateLinkForBasicUser <$> basicDuplicateTopicIds <*> problem
            | otherwise -> R.blank
