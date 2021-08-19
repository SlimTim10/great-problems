module SignIn
  ( widget
  ) where

import qualified Obelisk.Route.Frontend as Ob
import qualified Language.Javascript.JSaddle as JS
import qualified Reflex.Dom.Core as R
import qualified MyReflex.Dom.Widget.Basic as R'

import qualified Common.Route as Route
import qualified Inputs
import qualified Buttons
import qualified Util
import Global

widget
  :: forall t m js.
     ( R.DomBuilder t m
     , Ob.SetRoute t (Ob.R Route.FrontendRoute) m
     , Ob.RouteToUrl (Ob.R Route.FrontendRoute) m
     , R.Prerender js t m
     , R.PerformEvent t m
     , JS.MonadJSM (R.Performable m)
     )
  => m ()
widget = do
  R.elClass "div" "mt-10 flex justify-center" $ do
    R.elClass "div" "flex flex-col gap-4 w-80" $ do
      email <- R.elClass "div" "flex justify-between" $ do
        R.elClass "p" "font-normal text-brand-lg" $ R.text "Email"
        email <- Inputs.emailClass "border px-1"
        return email
      password <- R.elClass "div" "flex justify-between" $ do
        R.elClass "p" "font-normal text-brand-lg" $ R.text "Password"
        password <- Inputs.passwordClass "border px-1"
        return password
      Buttons.primary "Sign in"
      signIn :: R.Event t () <- R'.elAttrClass
        "button"
        ("type" =: "button")
        "bg-brand-primary rounded text-white font-medium px-3 h-10"
        $ do
        x <- R.button "Sign in"
        return x
      R.performEvent $ R.ffor (signIn) $ \x -> JS.liftJSM $ do
        Util.consoleLog ("Sign in button clicked" :: String)
      R.blank
