module Explore
  ( widget
  ) where

import qualified Control.Monad.Fix as Fix
import qualified Language.Javascript.JSaddle as JS
-- import qualified Obelisk.Route.Frontend as Ob
import qualified Reflex.Dom.Core as R
import qualified MyReflex.Dom.Widget.Basic as R'

import qualified Common.Api.Topic as Topic
-- import qualified Common.Route as Route
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
widget = do
  R.elClass "div" "bg-brand-light-gray flex justify-center py-4" $ do
    R.elClass "div" "max-w-screen-lg flex flex-col items-center" $ do
      R.elClass "p" "text-brand-lg font-light" $ R.text "Pick a topic"
      R.elClass "div" "flex justify-center flex-wrap" $ do
        onload :: R.Event t () <- R.getPostBuild
        let endpoint :: R.Event t Text = R.tagPromptlyDyn (R.constDyn "/api/topics/roots") onload
        response :: R.Event t (Maybe [Topic.Topic]) <- R.getAndDecode endpoint
        topics :: R.Dynamic t [Topic.Topic] <- R.holdDyn [] $ fromMaybe [] <$> response
        void $ R.simpleList topics topicWidget
  R.elClass "div" "my-6 flex justify-center" $ do
    R.elClass "div" "flex justify-around w-brand-screen-lg" $ do
      R'.elAttrClass "a" ("href" =: "/explore") "flex-1 border-b-2 border-brand-black" $ do
        R.elClass "p" "text-center text-brand-lg font-normal" $ R.text "Problems"
      R'.elAttrClass "a" ("href" =: "/explore/problem-sets") "flex-1 border-b border-brand-light-gray" $ do
        R.elClass "p" "text-center text-brand-lg text-brand-light-gray font-light" $ R.text "Problem sets"
  R.elClass "div" "flex justify-center" $ do
    R.elClass "div" "w-brand-screen-lg flex flex-col" $ do
      -- Mock problem
      let pTopics = ["Mathematics", "Calculus", "Rates"]
      let pSummary = "Find the present value of a continuous annuity at an annual rate of 2% compounded continuously for 4 years if the payment at time t is at the rate of $400 per year."
      let pDate = "2021-08-04"
      let pAuthor = "Bob"
      R'.elAttrClass "a" ("href" =: "/problems/1") "problem p-2 border border-brand-light-gray" $ do
        R.elClass "p" "text-brand-sm text-brand-gray" $ R.text (cs $ intercalate " > " pTopics)
        R.elClass "p" "problem-summary text-brand-primary font-medium" $ R.text pSummary
        R.elClass "p" "text-brand-sm text-brand-gray" $ R.text (cs $ "Updated " ++ pDate)
        R.elClass "div" "flex" $ do
          R.elClass "p" "text-brand-sm text-brand-gray mr-1" $ R.text "by"
          R'.elAttrClass "a" ("href" =: "/users/1") "hover:underline text-brand-sm text-brand-gray font-bold" $ R.text pAuthor

topicWidget
  :: forall t m.
  ( R.DomBuilder t m
  , R.MonadSample t m
  ) => R.Dynamic t Topic.Topic
  -> m ()
topicWidget topic = R.elClass "span" "m-2" $ do
  topic' <- R.sample . R.current $ topic
  -- Ob.routeLink (Route.FrontendRoute_ViewTopic :/ (Topic.id topic')) $ do
  R.elAttr "a" ("href" =: "/topics/1/problems") $ do
    Buttons.secondary (Topic.name topic')
