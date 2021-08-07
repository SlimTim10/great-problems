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
import qualified Common.Api.ProblemTile as ProblemTile
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
      response :: R.Event t (Maybe [ProblemTile.ProblemTile]) <- Util.getOnload "/api/problems"
      problemTiles :: R.Dynamic t [ProblemTile.ProblemTile] <- R.holdDyn [] $ fromMaybe [] <$> response
      void $ R.simpleList problemTiles problemTileWidget

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

problemTileWidget
  :: ( R.DomBuilder t m
     , R.PostBuild t m
     , Ob.SetRoute t (Ob.R Route.FrontendRoute) m
     , Ob.RouteToUrl (Ob.R Route.FrontendRoute) m
     , R.Prerender js t m
     )
  => R.Dynamic t ProblemTile.ProblemTile
  -> m ()
problemTileWidget problemTile = R.dyn_ $ R.ffor problemTile $ \(ProblemTile.ProblemTile problem topics author) -> do
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
