module Profile
  ( widget
  ) where

import qualified Reflex.Dom.Core as R
import qualified Obelisk.Route.Frontend as Ob

import qualified Common.Route as Route
import qualified Buttons
import Global

widget
  :: forall t m js.
     ( R.DomBuilder t m
     , Ob.SetRoute t (Ob.R Route.FrontendRoute) m
     , Ob.RouteToUrl (Ob.R Route.FrontendRoute) m
     , R.Prerender js t m
     )
  => m ()
widget = do
  R.elClass "div" "mt-10 flex justify-center" $ do
    Ob.routeLink (Route.FrontendRoute_SignOut :/ ()) $ do
      Buttons.secondary "Sign out"
