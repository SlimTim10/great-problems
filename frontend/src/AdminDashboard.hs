module AdminDashboard
  ( widget
  ) where

import qualified Language.Javascript.JSaddle as JS
import qualified Obelisk.Route.Frontend as Ob
import qualified Reflex.Dom.Core as R

import Common.Lib.Prelude
import qualified Common.Route as Route
import qualified AdminDashboard.Users
import qualified AdminDashboard.MetaSettings

widget
  :: forall t m js.
     ( R.DomBuilder t m
     , R.PostBuild t m
     , R.MonadHold t m
     , MonadFix m
     , JS.MonadJSM m
     , JS.MonadJSM (R.Performable m)
     , R.HasJSContext (R.Performable m)
     , R.PerformEvent t m
     , R.TriggerEvent t m
     , Ob.RouteToUrl (Ob.R Route.FrontendRoute) m
     , Ob.SetRoute t (Ob.R Route.FrontendRoute) m
     , R.Prerender js t m
     )
  => m ()
widget = do
  R.elClass "div" "flex justify-center" $ do
    R.elClass "div" "w-brand-screen-lg flex flex-col gap-10" $ do
      R.el "div" $ do
        AdminDashboard.Users.widget
      R.el "div" $ do
        AdminDashboard.MetaSettings.widget
