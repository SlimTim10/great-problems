module Topics
  ( widget
  ) where

import qualified Control.Monad.Fix as Fix
import qualified Language.Javascript.JSaddle as JS
import qualified Obelisk.Route.Frontend as Ob
import qualified Reflex.Dom.Core as R

import qualified Common.Api.Topic as Topic
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
  => Maybe Integer
  -> m ()
widget topicId = do
  R.elClass "div" "bg-brand-light-gray flex justify-center py-4" $ do
    R.elClass "div" "max-w-screen-lg flex flex-col items-center" $ do
      R.elClass "p" "text-brand-lg font-light" $ R.text "Pick a topic"
      R.elClass "div" "flex justify-center flex-wrap" $ do
        response :: R.Event t (Maybe [Topic.Topic]) <- case topicId of
          Nothing -> Util.getOnload "/api/topics?parent=null"
          Just tid -> Util.getOnload (cs $ "/api/topics?parent=" ++ show tid)
        topics :: R.Dynamic t [Topic.Topic] <- R.holdDyn [] $ fromMaybe [] <$> response
        void $ R.simpleList topics topicWidget
  where
    topicWidget topic = R.elClass "span" "m-2" $ do
      R.dyn_ $ R.ffor topic $ \t -> do
        Ob.routeLink
          (Route.FrontendRoute_Topics :/ (Topic.id t, Route.TopicsRoute_Problems :/ ())) $ do
          Buttons.secondary (Topic.name t)
