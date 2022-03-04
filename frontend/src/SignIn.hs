{-# LANGUAGE RankNTypes #-}
module SignIn
  ( widget
  ) where

import Common.Lib.Prelude

import qualified Language.Javascript.JSaddle as JS
import qualified Data.CaseInsensitive as CI
import qualified Web.KeyCode as Key
import qualified Reflex.Dom.Core as R
import qualified Obelisk.Route.Frontend as Ob

import qualified Common.Route as Route
import qualified Widget.Input as Input
import qualified Widget.Button as Button
import qualified Common.Api.Request.Auth as Auth
import qualified Common.Api.Error as Error
import qualified Frontend.Lib.Api as Api

widget
  :: forall t m js.
     ( R.DomBuilder t m
     , R.PerformEvent t m
     , JS.MonadJSM (R.Performable m)
     , R.HasJSContext (R.Performable m)
     , R.TriggerEvent t m
     , Ob.SetRoute t (Ob.R Route.FrontendRoute) m
     , R.MonadHold t m
     , R.PostBuild t m
     , Ob.RouteToUrl (Ob.R Route.FrontendRoute) m
     , R.Prerender js t m
     )
  => m ()
widget = do
  R.elClass "div" "mt-10 flex justify-center" $ do
    R.elClass "div" "flex flex-col gap-4 w-80" $ do
      
      emailInput <- R.elClass "div" "flex justify-between" $ do
        R.elClass "p" "font-normal text-brand-lg" $ R.text "Email"
        Input.rawEmailClass "border px-1"
      let email :: R.Dynamic t Text = R.value emailInput
      
      passwordInput <- R.elClass "div" "flex justify-between" $ do
        R.elClass "p" "font-normal text-brand-lg" $ R.text "Password"
        Input.rawPasswordClass "border px-1"
      let password :: R.Dynamic t Text = R.value passwordInput
      
      signInButton :: R.Event t () <- Button.primary' "Sign in"

      Ob.routeLink (Route.FrontendRoute_ForgotPassword :/ ()) $ do
        R.elClass "p" "text-blue-500" $ R.text "Forgot password?"

      Ob.routeLink (Route.FrontendRoute_ResendEmail :/ ()) $ do
        R.elClass "p" "text-blue-500" $ R.text "Resend activation email"

      let signIn = R.leftmost
            [ signInButton
            , R.keydown Key.Enter emailInput
            , R.keydown Key.Enter passwordInput
            ]

      response :: R.Event t (Either Error.Error ()) <- Api.request
        (R.zipDyn email password)
        signIn
        (Route.Api_SignIn :/ ())
        (\(email', password') -> Auth.Auth (CI.mk email') password')

      errorMessage :: R.Dynamic t (m ()) <- R.holdDyn R.blank
        $ R.ffor (R.filterLeft response)
        $ \e -> do
        R.elClass "p" "text-red-500" $ R.text (Error.message e)
        
      R.dyn_ errorMessage
      Ob.setRoute $ Route.FrontendRoute_Explore :/ Nothing <$ R.filterRight response

      R.blank
