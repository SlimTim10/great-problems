module Header
  ( widget
  ) where

import qualified Obelisk.Route.Frontend as Ob
import qualified Reflex.Dom.Core as R
import qualified MyReflex.Dom.Widget.Basic as R'

import qualified Common.Route as Route
import qualified Buttons as Buttons

import Global

widget
  :: forall t m js.
     ( R.DomBuilder t m
     , Ob.SetRoute t (Ob.R Route.FrontendRoute) m
     , Ob.RouteToUrl (Ob.R Route.FrontendRoute) m
     , R.Prerender js t m
     )
  => m ()
widget = R.elClass
  "header"
  "h-14 py-2 px-3 flex items-center justify-between border-b" $ do
    Ob.routeLink (Route.FrontendRoute_Main :/ ()) $ do
      R.elClass "p" "font-medium text-xl"
        $ R.text "Great Problems"
    Buttons.secondary "Explore"
    R'.elAttrClass
      "input"
      ("type" =: "text" <> "placeholder" =: "Search...")
      "border rounded h-8 w-1/2 px-1"
      $ R.blank
    R.el "div" $ do
      Buttons.primary "Create an account"
      Buttons.secondary "Sign in"
