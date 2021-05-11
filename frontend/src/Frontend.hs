module Frontend where

import Obelisk.Frontend
-- import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static

import Reflex.Dom.Core

-- import Common.Api
import Common.Route

import qualified Problem

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Great Problems"
      elAttr "meta" ("charset" =: "UTF-8") blank
      elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1.0") blank
      elAttr "link" ("href" =: "https://unpkg.com/tailwindcss@^2/dist/tailwind.min.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do
      elClass "div" "h-screen flex flex-col" $ do
        elClass "p" "text-2xl" $ text "Problem to Tex"
        prerender_ blank $ Problem.widget
  }
