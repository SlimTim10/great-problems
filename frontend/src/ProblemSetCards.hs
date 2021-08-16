module ProblemSetCards
  ( widget
  ) where

import qualified Data.CaseInsensitive as CI
import qualified Control.Monad.Fix as Fix
import qualified Language.Javascript.JSaddle as JS
import qualified Obelisk.Route.Frontend as Ob
import qualified Reflex.Dom.Core as R

import qualified Common.Api.Topic as Topic
import qualified Common.Api.ProblemSet as ProblemSet
import qualified Common.Api.ProblemSetCard as ProblemSetCard
import qualified Common.Api.User as User
import qualified Common.Route as Route
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
  => Maybe Integer
  -> m ()
widget topicId = do
  R.elClass "div" "flex justify-center" $ do
    R.elClass "div" "w-brand-screen-lg flex flex-col gap-2" $ do
      response :: R.Event t (Maybe [ProblemSetCard.ProblemSetCard]) <- case topicId of
        Nothing -> Util.getOnload "/api/problems-sets"
        Just tid -> Util.getOnload (cs $ "/api/topics/" ++ show tid ++ "/problem-sets")
      problemSetCards :: R.Dynamic t [ProblemSetCard.ProblemSetCard] <- R.holdDyn [] $ fromMaybe [] <$> response
      R.el "p" $ R.text "Under construction..."
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
            (Route.FrontendRoute_Topics :/ (tid, Route.TopicsRoute_ProblemSets :/ ())) $ do
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