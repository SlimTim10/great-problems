module ProblemCards
  ( widget
  ) where

import qualified Data.CaseInsensitive as CI
import qualified Control.Monad.Fix as Fix
import qualified Language.Javascript.JSaddle as JS
import qualified Obelisk.Route.Frontend as Ob
import qualified Reflex.Dom.Core as R

import qualified Common.Api.Topic as Topic
import qualified Common.Api.Problem as Problem
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
      response :: R.Event t (Maybe [Problem.Problem]) <- case topicId of
        Nothing -> Util.getOnload
          $ Route.apiHref
          $ Route.Api_Problems :/
          ( Nothing, Problem.getParamsToRouteQuery
            $ Problem.GetParams
            { Problem.gpExpand = Just ["author", "topic"]
            , Problem.gpInclude = Just Problem.TopicPath
            , Problem.gpTopic = Nothing
            }
          )
        Just tid -> Util.getOnload
          $ Route.apiHref
          $ Route.Api_Problems :/
          ( Nothing, Problem.getParamsToRouteQuery
            $ Problem.GetParams
            { Problem.gpExpand = Just ["author", "topic"]
            , Problem.gpInclude = Just Problem.TopicPath
            , Problem.gpTopic = Just tid
            }
          )
      problemCards :: R.Dynamic t [Problem.Problem] <- R.holdDyn [] $ fromMaybe [] <$> response
      void $ R.simpleList problemCards problemCardWidget

problemCardWidget
  :: ( R.DomBuilder t m
     , R.PostBuild t m
     , Ob.SetRoute t (Ob.R Route.FrontendRoute) m
     , Ob.RouteToUrl (Ob.R Route.FrontendRoute) m
     , R.Prerender js t m
     )
  => R.Dynamic t Problem.Problem
  -> m ()
problemCardWidget problemCard = do
  Util.dynFor problemCard $ \problem -> do
    let updatedAt = show $ Problem.updatedAt problem
    R.elClass "div" "p-2 border border-brand-light-gray flex flex-col gap-1" $ do
      R.elClass "div" "flex justify-between" $ do
        R.elClass "div" "flex" $ do
          let topics = fromMaybe [] $ Problem.topicPath problem
          forM_ (zip [0..] topics) $ \(n :: Integer, Topic.Topic tid name _) -> do
            unless (n == 0) $ do
              R.elClass "p" "text-brand-sm text-brand-gray mx-1" $ R.text ">"
            Ob.routeLink
              (Route.FrontendRoute_Topics :/ (tid, Route.TopicsRoute_Problems :/ ())) $ do
              R.elClass "p" "hover:underline text-brand-sm text-brand-gray" $ R.text name
        R.elClass "p" "text-brand-sm text-brand-gray" $ R.text (cs $ "#" ++ show (Problem.id problem))
      Ob.routeLink
        (Route.FrontendRoute_Problems :/
         (Problem.id problem, Route.ProblemsRoute_View :/ ())) $ do
        R.elClass "div" "group" $ do
          R.elClass "p" "text-brand-primary font-medium group-hover:underline" $ R.text (Problem.summary problem)
          R.elClass "p" "text-brand-sm text-brand-gray" $ R.text (cs $ "Updated " ++ updatedAt)
      R.elClass "div" "flex" $ do
        R.elClass "p" "text-brand-sm text-brand-gray mr-1" $ R.text "by"
        case Problem.author problem of
              Left authorId -> do
                -- TODO: handle this better
                R.el "p" $ R.text . cs $ "Author ID: " ++ show authorId
              Right author -> do
               Ob.routeLink (Route.FrontendRoute_ViewUser :/ User.id author) $ do
                 R.elClass "div" "hover:underline text-brand-sm text-brand-gray font-bold" $ do
                   R.text (CI.original $ User.full_name author)
