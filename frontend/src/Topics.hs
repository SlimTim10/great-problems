module Topics
  ( widget
  ) where

import Common.Lib.Prelude
import qualified Frontend.Lib.Util as Util

import qualified Control.Monad.Fix as Fix
import qualified Language.Javascript.JSaddle as JS
import qualified Obelisk.Route.Frontend as Ob
import qualified Reflex.Dom.Core as R

import qualified Common.Api.Topic as Topic
import qualified Common.Route as Route
import qualified Widget.Button as Button

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
      topicHierarchy :: R.Dynamic t [[Either Topic.Topic Topic.Topic]] <- case topicId of
        Nothing -> do
          response :: R.Event t (Maybe [Topic.Topic]) <- Util.getOnload
            $ Route.apiHref (Route.Api_Topics :/ ("parent" =: Just "null"))
          R.holdDyn []
            $ singleton
            <$> map Left
            <$> fromMaybe []
            <$> response
        Just tid -> do
          response :: R.Event t (Maybe [[Either Topic.Topic Topic.Topic]]) <- Util.getOnload
            $ Route.apiHref (Route.Api_TopicHierarchy :/ Just tid)
          R.holdDyn []
            $ filter ((> 0) . length)
            <$> fromMaybe []
            <$> response
      void $ R.simpleList (safeInit <$> topicHierarchy) topicsRow
      void $ R.simpleList (singleton . safeLast <$> topicHierarchy) topicsLastRow
  where
    topicsRow topics = do
      R.elClass "div" "flex justify-center flex-wrap w-full py-2 border-b border-brand-light-gray" $ do
        void $ R.simpleList topics topicWidget
    topicsLastRow topics = do
      R.elClass "div" "flex justify-center flex-wrap w-full py-2" $ do
        void $ R.simpleList topics topicWidget
    topicWidget topic = R.elClass "span" "m-2" $ do
      Util.dynFor topic $ \case
        Left t -> do
          Ob.routeLink
            (Route.FrontendRoute_Topics :/ (Topic.id t, Route.TopicsRoute_Problems :/ ())) $ do
            Button.secondary (Topic.name t)
        Right t -> do
          Ob.routeLink
            (Route.FrontendRoute_Topics :/ (Topic.id t, Route.TopicsRoute_Problems :/ ())) $ do
            Button.primary (Topic.name t)
    safeInit xs = if length xs == 0 then [] else init xs
    safeLast xs = if length xs == 0 then [] else last xs
    singleton x = [x]
