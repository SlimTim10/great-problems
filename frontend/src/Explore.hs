module Explore
  ( widget
  ) where

import qualified Control.Monad.Fix as Fix
import qualified Language.Javascript.JSaddle as JS
-- import qualified Obelisk.Route.Frontend as Ob
import qualified Reflex.Dom.Core as R
import qualified MyReflex.Dom.Widget.Basic as R'

import qualified Common.Api.Topic as Topic
import qualified Common.Api.Problem as Problem
import qualified Common.Api.ProblemTile as ProblemTile
-- import qualified Common.Route as Route
import qualified Buttons
import qualified Util
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
        response :: R.Event t (Maybe [Topic.Topic]) <- Util.getOnload "/api/topics/roots"
        topics :: R.Dynamic t [Topic.Topic] <- R.holdDyn [] $ fromMaybe [] <$> response
        void $ R.simpleList topics topicWidget
  R.elClass "div" "my-6 flex justify-center" $ do
    R.elClass "div" "flex justify-around w-brand-screen-lg" $ do
      R'.elAttrClass "a" ("href" =: "/explore") "flex-1 border-b-2 border-brand-black" $ do
        R.elClass "p" "text-center text-brand-lg font-normal" $ R.text "Problems"
      R'.elAttrClass "a" ("href" =: "/explore/problem-sets") "flex-1 border-b border-brand-light-gray" $ do
        R.elClass "p" "text-center text-brand-lg text-brand-light-gray font-light" $ R.text "Problem sets"
  R.elClass "div" "flex justify-center" $ do
    R.elClass "div" "w-brand-screen-lg flex flex-col gap-2" $ do
      response :: R.Event t (Maybe [ProblemTile.ProblemTile]) <- Util.getOnload "/api/problems"
      problemTiles :: R.Dynamic t [ProblemTile.ProblemTile] <- R.holdDyn [] $ fromMaybe [] <$> response
      void $ R.simpleList problemTiles problemTileWidget

topicWidget
  :: ( R.DomBuilder t m
     , R.MonadSample t m
     ) => R.Dynamic t Topic.Topic
  -> m ()
topicWidget topic = R.elClass "span" "m-2" $ do
  topic' <- R.sample . R.current $ topic
  -- TODO: change <a> to routeLink (need to add TopicProblems route first)
  -- Ob.routeLink (Route.FrontendRoute_TopicProblems :/ (Topic.id topic')) $ do
  R.elAttr "a" ("href" =: "/topics/1/problems") $ do
    Buttons.secondary (Topic.name topic')

problemTileWidget
  :: ( R.DomBuilder t m
     , R.MonadSample t m
     )
  => R.Dynamic t ProblemTile.ProblemTile
  -> m ()
problemTileWidget problemTile = do
  problemTile' <- R.sample . R.current $ problemTile
  let problem = ProblemTile.problem problemTile'
  let topics = ProblemTile.topics problemTile'
  -- TODO: add author to ProblemTile
  -- let author = ProblemTile.author problemTile'
  let topicNames = map (cs . Topic.name) topics
  let updatedAt = show $ Problem.updated_at problem
  let authorName = "Bob" -- Temporary mock
  -- TODO: routeLink instead of <a>
  R'.elAttrClass
    "a"
    ("href" =: cs ("/problems/" ++ show (Problem.id problem)))
    "p-2 border border-brand-light-gray flex flex-col gap-1 group"
    $ do
    R.elClass "div" "flex justify-between" $ do
      -- TODO: each topic name should be a link to /topics/:id/problems
      R.elClass "p" "text-brand-sm text-brand-gray" $ R.text (cs $ intercalate " > " topicNames)
      R.elClass "p" "text-brand-sm text-brand-gray" $ R.text (cs $ "#" ++ show (Problem.id problem))
    R.elClass "p" "text-brand-primary font-medium group-hover:underline" $ R.text (Problem.summary problem)
    R.elClass "p" "text-brand-sm text-brand-gray" $ R.text (cs $ "Updated " ++ updatedAt)
    R.elClass "div" "flex" $ do
      R.elClass "p" "text-brand-sm text-brand-gray mr-1" $ R.text "by"
      R'.elAttrClass
        "a"
        ("href" =: "/users/1")
        "hover:underline text-brand-sm text-brand-gray font-bold"
        $ R.text authorName
