module Explore
  ( widget
  ) where

import qualified Control.Monad.Fix as Fix
import qualified Language.Javascript.JSaddle as JS
import qualified Obelisk.Route.Frontend as Ob
import qualified Reflex.Dom.Core as R

import qualified Common.Api.Topic as Topic
import qualified Common.Route as Route
import qualified Buttons
import Global

widget
  :: forall t m.
  ( R.DomBuilder t m
  , R.PostBuild t m
  , JS.MonadJSM m
  , JS.MonadJSM (R.Performable m)
  , R.PerformEvent t m
  , R.HasJSContext (R.Performable m)
  , R.TriggerEvent t m
  , R.MonadHold t m
  , Fix.MonadFix m
  )
  => m ()
widget = R.elClass "div" "bg-brand-light-grey flex flex-col items-center py-2" $ do
  R.elClass "div" "max-w-screen-lg flex flex-col items-center" $ do
    R.elClass "p" "text-brand-large text-brand-light" $ R.text "Pick a topic"
    R.elClass "div" "flex justify-center flex-wrap" $ do
      onload :: R.Event t () <- R.getPostBuild
      let endpoint :: R.Event t Text = R.tagPromptlyDyn (R.constDyn "/api/topics/roots") onload
      response :: R.Event t (Maybe [Topic.Topic]) <- R.getAndDecode endpoint
      topics :: R.Dynamic t [Topic.Topic] <- R.holdDyn [] $ fromMaybe [] <$> response
      void $ R.simpleList topics topicWidget

topicWidget
  :: forall t m.
  ( R.DomBuilder t m
  , R.MonadSample t m
  ) => R.Dynamic t Topic.Topic
  -> m ()
topicWidget topic = R.elClass "span" "m-2" $ do
  topic' <- R.sample . R.current $ topic
  -- Ob.routeLink (Route.FrontendRoute_ViewTopic :/ (Topic.id topic')) $ do
  Buttons.secondary (Topic.name topic')
