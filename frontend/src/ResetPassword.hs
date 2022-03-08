module ResetPassword
  ( widget
  ) where

import Common.Lib.Prelude

import qualified Web.KeyCode as Key
import qualified Language.Javascript.JSaddle as JS
import qualified Obelisk.Route.Frontend as Ob
import qualified Reflex.Dom.Core as R

import qualified Widget.Input as Input
import qualified Widget.Button as Button
import qualified Widget.Spinner as Spinner
import qualified Common.Api.Error as Error
import qualified Common.Route as Route
import qualified Frontend.Lib.Api as Api
import qualified Common.Api.Request.ChangePassword as ChangePassword

widget
  :: forall t m js.
     ( R.DomBuilder t m
     , R.PerformEvent t m
     , JS.MonadJSM (R.Performable m)
     , R.HasJSContext (R.Performable m)
     , R.TriggerEvent t m
     , R.MonadHold t m
     , R.PostBuild t m
     , MonadFix m
     , Ob.RouteToUrl (Ob.R Route.FrontendRoute) m
     , Ob.SetRoute t (Ob.R Route.FrontendRoute) m
     , R.Prerender js t m
     )
  => R.Dynamic t Text -- ^ Reset secret
  -> m ()
widget secret = do
  R.elClass "div" "mt-10 flex justify-center" $ do
    R.elClass "div" "flex flex-col gap-4" $ mdo
      
      newPassword :: R.Dynamic t Text <- R.elClass "div" "flex justify-between gap-4" $ do
        R.elClass "p" "font-normal text-brand-lg" $ R.text "New password"
        Input.passwordClass "border px-1"

      confirmNewPasswordInput <- R.elClass "div" "flex justify-between gap-4" $ do
        R.elClass "p" "font-normal text-brand-lg" $ R.text "Confirm new password"
        Input.rawPasswordClass "border px-1"
      let confirmNewPassword :: R.Dynamic t Text = R.value confirmNewPasswordInput

      R.dyn_ passwordMatchError
      R.dyn_ passwordEmptyError

      changePasswordButton :: R.Event t () <- Button.primary' "Change my password"

      spinner <- Spinner.holdSmall triggerRequest
      status <- R.holdDyn R.blank . R.leftmost . map R.updated $ [spinner, message]
      R.dyn_ status

      let changePassword = R.leftmost
            [ changePasswordButton
            , R.keydown Key.Enter confirmNewPasswordInput
            ]

      let passwordMatch :: R.Dynamic t Bool = (==) <$> newPassword <*> confirmNewPassword
      passwordMatchError <- R.holdDyn R.blank $ R.ffor (R.tagPromptlyDyn passwordMatch changePassword) $ \case
        True -> R.blank
        False -> R.elClass "p" "text-red-500" $ R.text "Password must match"

      let passwordNotEmpty :: R.Dynamic t Bool = (/= "") <$> newPassword
      passwordEmptyError <- R.holdDyn R.blank $ R.ffor (R.tagPromptlyDyn passwordNotEmpty changePassword) $ \case
        True -> R.blank
        False -> R.elClass "p" "text-red-500" $ R.text "Password cannot be empty"

      let triggerRequest = () <$
            R.ffilter
            and
            (R.tagPromptlyDyn (R.distributeListOverDyn [passwordMatch, passwordNotEmpty]) changePassword)
            
      response :: R.Event t (Either Error.Error ()) <- Api.postRequest
        (R.zipDyn secret newPassword)
        triggerRequest
        (Route.Api_ChangePassword :/ ())
        (\(secret', newPassword') ->
           ChangePassword.ChangePassword
           (ChangePassword.ResetSecret secret')
           newPassword')
      
      message :: R.Dynamic t (m ()) <- R.holdDyn R.blank
        $ R.ffor response $ \case
        Left e -> do
          R.elClass "p" "text-red-500" $ R.text (Error.message e)
        Right _ -> do
          R.elClass "p" "text-green-600" $ R.text "Your password has been changed."
          R.elClass "p" "" $ do
            R.el "span" $ R.text "You may now "
            Ob.routeLink (Route.FrontendRoute_SignIn :/ ()) $ do
              R.elClass "span" "text-blue-500" $ R.text "sign in"
            R.el "span" $ R.text " with your new password "

      return ()
