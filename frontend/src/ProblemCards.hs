module ProblemCards
  ( widget
  , problemCardWidget
  , Options(..)
  ) where

import Common.Lib.Prelude
import qualified Frontend.Lib.Util as Util

import qualified Data.CaseInsensitive as CI
import qualified Control.Monad.Fix as Fix
import qualified Language.Javascript.JSaddle as JS
import qualified Obelisk.Route.Frontend as Ob
import qualified Reflex.Dom.Core as R

import qualified Common.Api.Topic as Topic
import qualified Common.Api.Problem as Problem
import qualified Common.Api.ProblemStatus as ProblemStatus
import qualified Common.Api.User as User
import qualified Common.Route as Route

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
      problems :: R.Dynamic t [Problem.Problem] <- getProblems topicId
      void $ R.simpleList problems (problemCardWidget defaultOptions)

getProblems
  :: ( R.PostBuild t m
     , JS.MonadJSM (R.Performable m)
     , R.PerformEvent t m
     , R.HasJSContext (R.Performable m)
     , R.TriggerEvent t m
     , R.MonadHold t m
     , JS.MonadJSM m
     )
  => Maybe Integer
  -> m (R.Dynamic t [Problem.Problem])
getProblems topicId = do
  response :: R.Event t (Maybe [Problem.Problem]) <- case topicId of
    Nothing -> Util.getOnload
      $ Route.apiHref
      $ Route.Api_Problems :/
      (Nothing, Problem.getParamsToRouteQuery Problem.getParamsDefault)
    Just tid -> Util.getOnload
      $ Route.apiHref
      $ Route.Api_Problems :/
      ( Nothing, Problem.getParamsToRouteQuery
        $ Problem.GetParams
        { Problem.gpTopic = Just tid
        , Problem.gpAuthor = Nothing
        , Problem.gpStatus = Just . fromIntegral . fromEnum $ ProblemStatus.Published
        }
      )
  R.holdDyn [] $ fromMaybe [] <$> response

data Options = Options
  { showAuthor :: Bool -- ^ Show the author
  , linkEdit :: Bool -- ^ Link to problem edit instead of view
  }

defaultOptions :: Options
defaultOptions = Options
  { showAuthor = True
  , linkEdit = False
  }

problemCardWidget
  :: ( R.DomBuilder t m
     , R.PostBuild t m
     , Ob.SetRoute t (Ob.R Route.FrontendRoute) m
     , Ob.RouteToUrl (Ob.R Route.FrontendRoute) m
     , R.Prerender js t m
     )
  => Options
  -> R.Dynamic t Problem.Problem
  -> m ()
problemCardWidget opt problemCard = do
  Util.dynFor problemCard $ \problem -> do
    let updatedAt = show $ Problem.updatedAt problem
    R.elClass "div" "p-2 border border-brand-light-gray flex flex-col gap-1" $ do
      R.elClass "div" "flex justify-between" $ do
        R.elClass "div" "flex" $ do
          let topics = Problem.topicPath problem
          forM_ (zip [0..] topics) $ \(n :: Integer, Topic.Topic tid name _) -> do
            unless (n == 0) $ do
              R.elClass "p" "text-brand-sm text-brand-gray mx-1" $ R.text ">"
            Ob.routeLink
              (Route.FrontendRoute_Topics :/ (tid, Route.TopicsRoute_Problems :/ ())) $ do
              R.elClass "p" "hover:underline text-brand-sm text-brand-gray" $ R.text name
        R.elClass "p" "text-brand-sm text-brand-gray" $ R.text (cs $ "#" ++ show (Problem.id problem))
      let linkProblem = if linkEdit opt
            then Ob.routeLink
                 (Route.FrontendRoute_Problems :/
                  (Problem.id problem, Route.ProblemsRoute_Edit :/ ()))
            else Ob.routeLink
                 (Route.FrontendRoute_Problems :/
                  (Problem.id problem, Route.ProblemsRoute_View :/ ()))
      linkProblem $ do
        R.elClass "div" "group" $ do
          R.elClass "p" "text-brand-primary font-medium group-hover:underline" $ R.text (Problem.summary problem)
          R.elClass "p" "text-brand-sm text-brand-gray" $ R.text (cs $ "Updated " ++ updatedAt)
      when (showAuthor opt) $ do
        R.elClass "div" "flex" $ do
          R.elClass "p" "text-brand-sm text-brand-gray mr-1" $ R.text "by"
          let author = Problem.author problem
          Ob.routeLink (Route.FrontendRoute_ViewUser :/ User.id author) $ do
            R.elClass "div" "hover:underline text-brand-sm text-brand-gray font-bold" $ do
              R.text (CI.original $ User.fullName author)
