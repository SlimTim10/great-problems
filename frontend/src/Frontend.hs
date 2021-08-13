module Frontend where

import qualified Obelisk.Frontend as Ob
import qualified Obelisk.Route.Frontend as Ob
import qualified Obelisk.Generated.Static as Ob
import qualified Reflex.Dom.Core as R

import qualified Common.Route as Route
import qualified Problem
import qualified Home
import qualified Header
import qualified Topics
import qualified Tabs
import qualified ProblemCards
import qualified ProblemSetCards
import Global

frontend :: Ob.Frontend (Ob.R Route.FrontendRoute)
frontend = Ob.Frontend
  { Ob._frontend_head = do
      R.el "title" $ R.text "Great Problems"
      R.elAttr "meta" ("charset" =: "UTF-8") R.blank
      R.elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1.0") R.blank
      R.elAttr "link" ("rel" =: "preconnect" <> "href" =: "https://fonts.googleapis.com") R.blank
      R.elAttr "link" ("rel" =: "preconnect" <> "href" =: "https://fonts.gstatic.com" <> "crossorigin" =: "") R.blank
      R.elAttr "link" ("href" =: "https://fonts.googleapis.com/css2?family=Roboto:wght@300;400;500&display=swap" <> "rel" =: "stylesheet") R.blank
      R.elAttr "link" ("href" =: "https://unpkg.com/tailwindcss@^2/dist/tailwind.min.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") R.blank
      R.elAttr "link" ("href" =: Ob.static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") R.blank
  , Ob._frontend_body = Ob.subRoute_ $ \case
      Route.FrontendRoute_Home -> do
        Header.widget
        R.el "p" $ R.text "(placeholder for marketing page)"
        R.el "p" $ R.text "Main page"
        Ob.routeLink (Route.FrontendRoute_New :/ ()) $ do
          R.elClass "p" "border-2 border-green-500 w-max" $ R.text "New problem"
        R.prerender_ R.blank $ Home.widget
      Route.FrontendRoute_Explore -> do
        Header.widget
        R.prerender_ R.blank $ do
          Topics.widget Nothing
          path :: R.Dynamic t (Maybe (Ob.R Route.ExploreRoute)) <- Ob.askRoute
          R.dyn_ $ R.ffor path $ \case
            Just (Route.ExploreRoute_Problems :/ ()) -> do
              Tabs.widget "Problems"
              ProblemCards.widget Nothing
            Just (Route.ExploreRoute_ProblemSets :/ ()) -> do
              Tabs.widget "Problem sets"
              ProblemSetCards.widget Nothing
            _ -> do
              Tabs.widget "Problems"
              ProblemCards.widget Nothing
      Route.FrontendRoute_Register -> do
        Header.widget
      Route.FrontendRoute_SignIn -> do
        Header.widget
      Route.FrontendRoute_New -> do
        Header.widget
        R.elClass "div" "h-screen flex flex-col" $ do
          R.elClass "p" "text-2xl" $ R.text "Problem to Tex"
          R.prerender_ R.blank $ Problem.widget
      Route.FrontendRoute_ViewProblem -> do
        Header.widget
        problemId :: R.Dynamic t Integer <- Ob.askRoute
        R.el "p" $ R.text "Single problem"
        R.el "p" $ R.display problemId
      Route.FrontendRoute_ViewProblemSet -> do
        Header.widget
        problemSetId :: R.Dynamic t Integer <- Ob.askRoute
        R.el "p" $ R.text "Single problem set"
        R.el "p" $ R.display problemSetId
      Route.FrontendRoute_Topics -> do
        Header.widget
        R.el "p" $ R.text "Problems belonging to a topic"
        r :: R.Dynamic t (Integer, Maybe (Ob.R Route.TopicsRoute)) <- Ob.askRoute
        R.el "p" $ R.display r
        R.el "span" $ R.text "topic id: "
        R.display (fst <$> r)
      Route.FrontendRoute_ViewUser -> do
        Header.widget
        userId :: R.Dynamic t Integer <- Ob.askRoute
        R.el "p" $ R.text "Single user"
        R.el "p" $ R.display userId
  }
