{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
module Frontend where

import Obelisk.Route ( pattern (:/) )
import qualified Obelisk.Frontend as Ob
import qualified Obelisk.Route.Frontend as Ob
import qualified Obelisk.Generated.Static as Ob
import qualified Reflex.Dom.Core as R

import qualified Common.Route as Route
import qualified Problem
import qualified Home
import Global

frontend :: Ob.Frontend (Ob.R Route.FrontendRoute)
frontend = Ob.Frontend
  { Ob._frontend_head = do
      R.el "title" $ R.text "Great Problems"
      R.elAttr "meta" ("charset" =: "UTF-8") R.blank
      R.elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1.0") R.blank
      R.elAttr "link" ("href" =: "https://unpkg.com/tailwindcss@^2/dist/tailwind.min.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") R.blank
      R.elAttr "link" ("href" =: Ob.static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") R.blank
  , Ob._frontend_body = Ob.subRoute_ $ \case
      Route.FrontendRoute_Main -> do
        R.el "p" $ R.text "Main page"
        Ob.routeLink (Route.FrontendRoute_New :/ ()) $ R.text "New problem"
        R.prerender_ R.blank $ Home.widget
      Route.FrontendRoute_New -> do
        R.elClass "div" "h-screen flex flex-col" $ do
          R.elClass "p" "text-2xl" $ R.text "Problem to Tex"
          R.prerender_ R.blank $ Problem.widget
  }
