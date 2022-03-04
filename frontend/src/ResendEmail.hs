module ResendEmail
  ( widget
  ) where

import Common.Lib.Prelude

import qualified Data.Text as T
import qualified Data.CaseInsensitive as CI
import qualified Web.KeyCode as Key
import qualified Language.Javascript.JSaddle as JS
import qualified Reflex.Dom.Core as R

import qualified Widget.Input as Input
import qualified Widget.Button as Button
import qualified Common.Api.Error as Error
import qualified Common.Route as Route
import qualified Layout.CenteredNarrow
import qualified Frontend.Lib.Api as Api
import qualified Common.Api.Request.ResendEmail as ResendEmail

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

    resendEmailButton :: R.Event t () <- Button.primary' "Resend activation email"

    let resendEmail =
          fmap (const ())
          $ R.ffilter (not . T.null)
          $ R.tagPromptlyDyn email
          $ R.leftmost [resendEmailButton, R.keydown Key.Enter emailInput]

    response :: Api.Response t () <- Api.request
      email
      resendEmail
      (Route.Api_ResendEmail :/ ())
      (\email' -> ResendEmail.ResendEmail (CI.mk email'))

    resendEmailError :: R.Dynamic t (m ()) <- R.holdDyn R.blank $
      R.ffor (Api.resError response) $ maybe R.blank $
      \e -> do
        R.elClass "p" "text-red-500" $ R.text (Error.message e)
    R.dyn_ resendEmailError

    resendEmailSuccess :: R.Dynamic t (m ()) <- R.holdDyn R.blank $
      R.ffor (Api.resSuccess response) $ maybe R.blank $
      \_ -> do
        R.elClass "p" "text-green-600" $
          R.text "We'll send you an email in 5 minutes. Open it up to activate your account."
    R.dyn_ resendEmailSuccess

    R.blank
