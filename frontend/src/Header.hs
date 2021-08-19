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
widget = R.elClass "header" "h-14 py-2 px-3 flex items-center justify-between border-b" $ do
  Ob.routeLink (Route.FrontendRoute_Home :/ ()) $ do
    R.elClass "p" "font-medium text-xl"
      $ R.text "Great Problems"
  Ob.routeLink (Route.FrontendRoute_Explore :/ Nothing) $ do
    Buttons.secondary "Explore"
  R'.elAttrClass
    "input"
    ("type" =: "text" <> "placeholder" =: "Search...")
    "border rounded h-8 w-1/2 px-1"
    $ R.blank
  R.el "div" $ do
    R.elClass "span" "pr-2" $ do
      Ob.routeLink (Route.FrontendRoute_Register :/ ()) $ do
        Buttons.primary "Create an account"
    Ob.routeLink (Route.FrontendRoute_SignIn :/ ()) $ do
      Buttons.secondary "Sign in"
