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
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do
      el "h1" $ text "Problem to Tex"

      prerender_ blank $ Problem.widget
  }
