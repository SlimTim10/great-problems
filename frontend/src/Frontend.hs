module Frontend where

import Common.Lib.Prelude
import qualified Frontend.Lib.Util as Util

import qualified Obelisk.Frontend as Ob
import qualified Obelisk.Route.Frontend as Ob
import qualified Obelisk.Generated.Static as Ob
import qualified Reflex.Dom.Core as R

import qualified Common.Route as Route
import qualified Problem.View
import qualified Problem.Edit
import qualified Problem.Duplicate
import qualified Home
import qualified Header
import qualified Topics
import qualified Tabs
import qualified ProblemCards
import qualified ProblemSetCards
import qualified Register
import qualified VerifyEmail
import qualified SignIn
import qualified SignOut
import qualified ForgotPassword
import qualified ResetPassword
import qualified ResendEmail
import qualified Settings
import qualified AdminArea

frontend :: Ob.Frontend (Ob.R Route.FrontendRoute)
frontend = Ob.Frontend
  { Ob._frontend_head = do
      R.el "title" $ R.text "Great Problems"
      R.elAttr "meta" ("charset" =: "UTF-8") R.blank
      R.elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1.0") R.blank
      R.elAttr "link" ("rel" =: "preconnect" <> "href" =: "https://fonts.googleapis.com") R.blank
      R.elAttr "link" ("rel" =: "preconnect" <> "href" =: "https://fonts.gstatic.com" <> "crossorigin" =: "") R.blank
      R.elAttr "link" ("href" =: "https://fonts.googleapis.com/css2?family=Roboto:wght@300;400;500&display=swap" <> "rel" =: "stylesheet") R.blank
      R.elAttr "link" ("href" =: Ob.static @"tailwind.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") R.blank
      R.elAttr "link" ("href" =: Ob.static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") R.blank
  , Ob._frontend_body = R.prerender_ R.blank $ Ob.subRoute_ $ \case
      Route.FrontendRoute_Home -> do
        Header.widget
        Home.widget
      Route.FrontendRoute_Explore -> do
        Header.widget
        Topics.widget Nothing
        path :: R.Dynamic t (Maybe (Ob.R Route.ExploreRoute)) <- Ob.askRoute
        Util.dynFor path $ \case
          Just (Route.ExploreRoute_Problems :/ ()) -> do
            Tabs.widget Tabs.Problems
            ProblemCards.widget Nothing
          Just (Route.ExploreRoute_ProblemSets :/ ()) -> do
            Tabs.widget Tabs.ProblemSets
            ProblemSetCards.widget Nothing
          _ -> do
            Tabs.widget Tabs.Problems
            ProblemCards.widget Nothing
      Route.FrontendRoute_Register -> do
        Header.widget
        R.elClass "div" "bg-brand-light-gray flex justify-center py-4" $ do
          R.elClass "p" "text-brand-lg font-light" $ R.text "Register"
        Register.widget
      Route.FrontendRoute_VerifyEmail -> do
        Header.widget
        secret :: R.Dynamic t Text <- Ob.askRoute
        VerifyEmail.widget secret
      Route.FrontendRoute_SignIn -> do
        Header.widget
        R.elClass "div" "bg-brand-light-gray flex justify-center py-4" $ do
          R.elClass "p" "text-brand-lg font-light" $ R.text "Sign in"
        SignIn.widget
      Route.FrontendRoute_SignOut -> do
        SignOut.widget
      Route.FrontendRoute_ForgotPassword -> do
        Header.widget
        ForgotPassword.widget
      Route.FrontendRoute_ResetPassword -> do
        Header.widget
        secret :: R.Dynamic t Text <- Ob.askRoute
        ResetPassword.widget secret
      Route.FrontendRoute_ResendEmail -> do
        Header.widget
        ResendEmail.widget
      Route.FrontendRoute_Settings -> do
        Header.widget
        R.elClass "div" "bg-brand-light-gray flex justify-center py-4" $ do
          R.elClass "p" "text-brand-lg font-light" $ R.text "Settings"
        Settings.widget
      Route.FrontendRoute_NewProblem -> do
        R.elClass "div" "h-screen flex flex-col gap-3" $ do
          R.elClass "div" "flex-none"
            Header.widget
          R.elClass "div" "flex-1 mx-2 flex justify-center" $ do
            Problem.Edit.widget Nothing
      Route.FrontendRoute_DuplicateProblem -> do
        problemId :: R.Dynamic t Integer <- Ob.askRoute
        Util.dynFor problemId $ \pId -> Problem.Duplicate.widget pId
      Route.FrontendRoute_Problems -> do
        path :: R.Dynamic t (Integer, Ob.R Route.ProblemsRoute) <- Ob.askRoute
        Util.dynFor path $ \(problemId, route) -> do
          case route of
            Route.ProblemsRoute_View :/ () -> do
              R.elClass "div" "h-screen flex flex-col" $ do
                R.elClass "div" "flex-none"
                  Header.widget
                Problem.View.widget problemId
            Route.ProblemsRoute_Edit :/ () -> do
              R.elClass "div" "h-screen flex flex-col gap-3" $ do
                R.elClass "div" "flex-none"
                  Header.widget
                R.elClass "div" "flex-1 mx-2 flex justify-center" $ do
                  Problem.Edit.widget $ Just problemId
            _ -> pure () -- Type refinement through unification
      Route.FrontendRoute_ViewProblemSet -> do
        Header.widget
        problemSetId :: R.Dynamic t Integer <- Ob.askRoute
        R.el "p" $ R.text "Single problem set"
        R.el "p" $ R.display problemSetId
      Route.FrontendRoute_Topics -> do
        Header.widget
        path :: R.Dynamic t (Integer, Ob.R Route.TopicsRoute) <- Ob.askRoute
        Util.dynFor path $ \(topicId, route) -> do
          Topics.widget $ Just topicId
          case route of
            Route.TopicsRoute_Problems :/ () -> do
              Tabs.widget Tabs.Problems
              ProblemCards.widget $ Just topicId
            Route.TopicsRoute_ProblemSets :/ () -> do
              Tabs.widget Tabs.ProblemSets
              ProblemCards.widget $ Just topicId
            _ -> pure () -- Type refinement through unification
      Route.FrontendRoute_Profile -> do
        Header.widget
        userId :: R.Dynamic t Integer <- Ob.askRoute
        R.el "p" $ R.text "User profile"
        R.el "p" $ R.display userId
      Route.FrontendRoute_Admin -> do
        Header.widget
        AdminArea.widget
  }
