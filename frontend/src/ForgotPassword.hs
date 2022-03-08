module ForgotPassword
  ( widget
  ) where

import Common.Lib.Prelude

import qualified Data.CaseInsensitive as CI
import qualified Web.KeyCode as Key
import qualified Language.Javascript.JSaddle as JS
import qualified Reflex.Dom.Core as R

import qualified Widget.Input as Input
import qualified Widget.Button as Button
import qualified Widget.Spinner as Spinner
import qualified Common.Api.Error as Error
import qualified Common.Route as Route
import qualified Common.Api.User as User
import qualified Frontend.Lib.Api as Api
import qualified Layout.CenteredNarrow

widget
  :: forall t m.
     ( R.DomBuilder t m
     , R.PerformEvent t m
     , JS.MonadJSM (R.Performable m)
     , R.HasJSContext (R.Performable m)
     , R.TriggerEvent t m
     , R.MonadHold t m
     , R.PostBuild t m
     )
  => m ()
widget = do
  Layout.CenteredNarrow.widget $ do
      
    emailInput <- R.elClass "div" "flex justify-between" $ do
      R.elClass "p" "font-normal text-brand-lg" $ R.text "Email"
      Input.rawEmailClass "border px-1"
    let email :: R.Dynamic t Text = R.value emailInput

    resetPasswordButton :: R.Event t () <- Button.primary' "Reset password"

    let resetPassword = R.leftmost
          [ resetPasswordButton
          , R.keydown Key.Enter emailInput
          ]

    response :: R.Event t (Either Error.Error ()) <- Api.postRequest
      email
      resetPassword
      (Route.Api_ResetPassword :/ ())
      (\email' -> User.ResetPasswordRequest (CI.mk email'))

    message :: R.Dynamic t (m ()) <- R.holdDyn R.blank
      $ R.ffor response $ \case
      Left e -> do
        R.elClass "p" "text-red-500" $ R.text (Error.message e)
      Right _ -> do
        R.elClass "p" "text-green-500" $ R.text "You should receive an email in 5 minutes allowing you to reset your password."

    spinner <- Spinner.holdSmall resetPassword
    status <- R.holdDyn R.blank . R.leftmost . map R.updated $ [spinner, message]
    R.dyn_ status
