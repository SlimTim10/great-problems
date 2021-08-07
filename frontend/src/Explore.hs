module Explore
  ( widget
  ) where

import qualified Data.CaseInsensitive as CI
import qualified Control.Monad.Fix as Fix
import qualified Language.Javascript.JSaddle as JS
import qualified Obelisk.Route.Frontend as Ob
import qualified Reflex.Dom.Core as R

import qualified Common.Api.Topic as Topic
import qualified Common.Api.Problem as Problem
import qualified Common.Api.ProblemCard as ProblemCard
import qualified Common.Api.ProblemSet as ProblemSet
import qualified Common.Api.ProblemSetCard as ProblemSetCard
import qualified Common.Api.User as User
import qualified Common.Route as Route
import qualified Buttons
import qualified Util
import Global

widget
  :: forall t m js.
     ( R.DomBuilder t m
     , R.PostBuild t m
     , JS.MonadJSM m
     , JS.MonadJSM (R.Performable m)
     , R.PerformEvent t m
     , R.HasJSContext (R.Performable m)
     , R.TriggerEvent t m
     , R.MonadHold t m
     , Fix.MonadFix m
     , Ob.SetRoute t (Ob.R Route.FrontendRoute) m
     , Ob.RouteToUrl (Ob.R Route.FrontendRoute) m
     , Ob.Routed t (Maybe (Ob.R Route.ExploreRoute)) m
     , R.Prerender js t m
     )
  => m ()
widget = do
  R.elClass "div" "bg-brand-light-gray flex justify-center py-4" $ do
    R.elClass "div" "max-w-screen-lg flex flex-col items-center" $ do
      R.elClass "p" "text-brand-lg font-light" $ R.text "Pick a topic"
      R.elClass "div" "flex justify-center flex-wrap" $ do
        response :: R.Event t (Maybe [Topic.Topic]) <- Util.getOnload "/api/topics/roots"
        topics :: R.Dynamic t [Topic.Topic] <- R.holdDyn [] $ fromMaybe [] <$> response
        void $ R.simpleList topics topicWidget
  R.elClass "div" "my-6 flex justify-center" $ do
    R.elClass "div" "flex justify-around w-brand-screen-lg" $ do
      R.elClass "div" "flex-1 border-b-2 border-brand-black" $ do
        Ob.routeLink (Route.FrontendRoute_Explore :/ (Just (Route.ExploreRoute_Problems :/ ()))) $ do
          R.elClass "p" "text-center text-brand-lg font-normal" $ R.text "Problems"
      R.elClass "div" "flex-1 border-b border-brand-light-gray" $ do
        Ob.routeLink (Route.FrontendRoute_Explore :/ (Just (Route.ExploreRoute_ProblemSets :/ ()))) $ do
          R.elClass "p" "text-center text-brand-lg text-brand-light-gray font-light" $ R.text "Problem sets"
  R.elClass "div" "flex justify-center" $ do
    R.elClass "div" "w-brand-screen-lg flex flex-col gap-2" $ do
      path :: R.Dynamic t (Maybe (Ob.R Route.ExploreRoute)) <- Ob.askRoute
      R.dyn_ $ R.ffor path $ \case
        Nothing -> problemCardsWidget
        Just (Route.ExploreRoute_Problems :/ ()) -> problemCardsWidget
        Just (Route.ExploreRoute_ProblemSets :/ ()) -> problemSetCardsWidget

topicWidget
  :: ( R.DomBuilder t m
     , Ob.SetRoute t (Ob.R Route.FrontendRoute) m
     , Ob.RouteToUrl (Ob.R Route.FrontendRoute) m
     , R.Prerender js t m
     , R.PostBuild t m
     ) => R.Dynamic t Topic.Topic
  -> m ()
topicWidget topic = R.elClass "span" "m-2" $ do
  R.dyn_ $ R.ffor topic $ \t -> do
    Ob.routeLink
      (Route.FrontendRoute_Topics :/ (Topic.id t, Just (Route.TopicsRoute_Problems :/ ()))) $ do
      Buttons.secondary (Topic.name t)

problemCardsWidget
  :: ( R.DomBuilder t m
     , R.TriggerEvent t m
     , R.HasJSContext (R.Performable m)
     , R.PerformEvent t m
     , R.PostBuild t m
     , JS.MonadJSM m
     , JS.MonadJSM (R.Performable m)
     , R.MonadHold t m
     , Fix.MonadFix m
     , Ob.SetRoute t (Ob.R Route.FrontendRoute) m
     , Ob.RouteToUrl (Ob.R Route.FrontendRoute) m
     , R.Prerender js t m
     )
  => m ()
problemCardsWidget = do
  response :: R.Event t (Maybe [ProblemCard.ProblemCard]) <- Util.getOnload "/api/problems"
  problemCards :: R.Dynamic t [ProblemCard.ProblemCard] <- R.holdDyn [] $ fromMaybe [] <$> response
  void $ R.simpleList problemCards problemCardWidget

problemCardWidget
  :: ( R.DomBuilder t m
     , R.PostBuild t m
     , Ob.SetRoute t (Ob.R Route.FrontendRoute) m
     , Ob.RouteToUrl (Ob.R Route.FrontendRoute) m
     , R.Prerender js t m
     )
  => R.Dynamic t ProblemCard.ProblemCard
  -> m ()
problemCardWidget problemCard = R.dyn_ $ R.ffor problemCard $ \(ProblemCard.ProblemCard problem topics author) -> do
  let updatedAt = show $ Problem.updated_at problem
  R.elClass "div" "p-2 border border-brand-light-gray flex flex-col gap-1" $ do
    R.elClass "div" "flex justify-between" $ do
      R.elClass "div" "flex" $ do
        forM_ (zip [0..] topics) $ \(n :: Integer, Topic.Topic tid name _) -> do
          unless (n == 0) $ do
            R.elClass "p" "text-brand-sm text-brand-gray mx-1" $ R.text ">"
          Ob.routeLink
            (Route.FrontendRoute_Topics :/ (tid, Just (Route.TopicsRoute_Problems :/ ()))) $ do
            R.elClass "p" "hover:underline text-brand-sm text-brand-gray" $ R.text name
      R.elClass "p" "text-brand-sm text-brand-gray" $ R.text (cs $ "#" ++ show (Problem.id problem))
    Ob.routeLink (Route.FrontendRoute_ViewProblem :/ (Problem.id problem)) $ do
      R.elClass "div" "group" $ do
        R.elClass "p" "text-brand-primary font-medium group-hover:underline" $ R.text (Problem.summary problem)
        R.elClass "p" "text-brand-sm text-brand-gray" $ R.text (cs $ "Updated " ++ updatedAt)
    R.elClass "div" "flex" $ do
      R.elClass "p" "text-brand-sm text-brand-gray mr-1" $ R.text "by"
      Ob.routeLink (Route.FrontendRoute_ViewUser :/ (User.id author)) $ do
        R.elClass "div" "hover:underline text-brand-sm text-brand-gray font-bold" $ do
          R.text (CI.original $ User.full_name author)

problemSetCardsWidget
  :: ( R.DomBuilder t m
     , R.TriggerEvent t m
     , R.HasJSContext (R.Performable m)
     , R.PerformEvent t m
     , R.PostBuild t m
     , JS.MonadJSM m
     , JS.MonadJSM (R.Performable m)
     , R.MonadHold t m
     , Fix.MonadFix m
     , Ob.SetRoute t (Ob.R Route.FrontendRoute) m
     , Ob.RouteToUrl (Ob.R Route.FrontendRoute) m
     , R.Prerender js t m
     )
  => m ()
problemSetCardsWidget = do
  response :: R.Event t (Maybe [ProblemSetCard.ProblemSetCard]) <- Util.getOnload "/api/problem-sets"
  problemSetCards :: R.Dynamic t [ProblemSetCard.ProblemSetCard] <- R.holdDyn [] $ fromMaybe [] <$> response
  void $ R.simpleList problemSetCards problemSetCardWidget

problemSetCardWidget
  :: ( R.DomBuilder t m
     , R.PostBuild t m
     , Ob.SetRoute t (Ob.R Route.FrontendRoute) m
     , Ob.RouteToUrl (Ob.R Route.FrontendRoute) m
     , R.Prerender js t m
     )
  => R.Dynamic t ProblemSetCard.ProblemSetCard
  -> m ()
problemSetCardWidget problemSetCard = R.dyn_ $ R.ffor problemSetCard $ \(ProblemSetCard.ProblemSetCard problemSet topics author) -> do
  let updatedAt = show $ ProblemSet.updated_at problemSet
  R.elClass "div" "p-2 border border-brand-light-gray flex flex-col gap-1" $ do
    R.elClass "div" "flex justify-between" $ do
      R.elClass "div" "flex" $ do
        forM_ (zip [0..] topics) $ \(n :: Integer, Topic.Topic tid name _) -> do
          unless (n == 0) $ do
            R.elClass "p" "text-brand-sm text-brand-gray mx-1" $ R.text ">"
          Ob.routeLink
            (Route.FrontendRoute_Topics :/ (tid, Just (Route.TopicsRoute_ProblemSets :/ ()))) $ do
            R.elClass "p" "hover:underline text-brand-sm text-brand-gray" $ R.text name
      R.elClass "p" "text-brand-sm text-brand-gray" $ R.text (cs $ "#" ++ show (ProblemSet.id problemSet))
    Ob.routeLink (Route.FrontendRoute_ViewProblemSet :/ (ProblemSet.id problemSet)) $ do
      R.elClass "div" "group" $ do
        R.elClass "p" "text-brand-primary font-medium group-hover:underline" $ R.text (ProblemSet.summary problemSet)
        R.elClass "p" "text-brand-sm text-brand-gray" $ R.text (cs $ "Updated " ++ updatedAt)
    R.elClass "div" "flex" $ do
      R.elClass "p" "text-brand-sm text-brand-gray mr-1" $ R.text "by"
      Ob.routeLink (Route.FrontendRoute_ViewUser :/ (User.id author)) $ do
        R.elClass "div" "hover:underline text-brand-sm text-brand-gray font-bold" $ do
          R.text (CI.original $ User.full_name author)

