module ForgotPassword
  ( widget
  ) where

import Common.Lib.Prelude

import qualified Data.CaseInsensitive as CI
import qualified Web.KeyCode as Key
import qualified Language.Javascript.JSaddle as JS
import qualified Obelisk.Generated.Static as Ob
import qualified Reflex.Dom.Core as R

import qualified Widget.Input as Input
import qualified Widget.Button as Button
import qualified Common.Api.Error as Error
import qualified Common.Route as Route
import qualified Common.Api.User as User
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

    response <- resetPasswordAttempt email resetPassword
    let err :: R.Event t (Either Text ()) = maybeToEither Error.message <$> response

    spinner <- R.holdDyn R.blank $ R.ffor resetPassword . const
      $ R.elAttr "img" ("src" =: Ob.static @"small_spinner.svg" <> "width" =: "30" <> "alt" =: "loading") $ R.blank
    responseEl <- R.holdDyn R.blank . R.ffor err $ \case
      Left e -> R.elClass "p" "text-red-500" $ R.text e
      Right _ -> R.elClass "p" "text-green-500" $ R.text "You should receive an email in 5 minutes allowing you to reset your password."
    status <- R.holdDyn R.blank . R.leftmost . map R.updated $ [spinner, responseEl]
    R.dyn_ status
  where
    resetPasswordAttempt
      :: R.Dynamic t Text -- ^ Email
      -> R.Event t () -- ^ Event to trigger request
      -> m (R.Event t (Maybe Error.Error))
    resetPasswordAttempt email resetPassword = do
      let ev :: R.Event t Text = R.tagPromptlyDyn email resetPassword
      r <- R.performRequestAsync $ R.ffor ev $ \email' -> do
        let url = Route.apiHref $ Route.Api_ResetPassword :/ ()
        R.postJson url $ User.ResetPasswordRequest (CI.mk email')
      return $ R.decodeXhrResponse <$> r
