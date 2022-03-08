module Register
  ( widget
  ) where

import Common.Lib.Prelude

import qualified Language.Javascript.JSaddle as JS
import qualified Web.KeyCode as Key
import qualified Data.CaseInsensitive as CI
import qualified Reflex.Dom.Core as R

import qualified Common.Route as Route
import qualified Widget.Input as Input
import qualified Widget.Button as Button
import qualified Widget.Spinner as Spinner
import qualified Frontend.Lib.Api as Api
import qualified Common.Api.Error as Error
import qualified Common.Api.Request.Register as Register

widget
  :: forall t m.
     ( R.DomBuilder t m
     , R.PerformEvent t m
     , JS.MonadJSM (R.Performable m)
     , R.HasJSContext (R.Performable m)
     , R.TriggerEvent t m
     , R.MonadHold t m
     , MonadFix m
     , R.PostBuild t m
     )
  => m ()
widget = do
  R.elClass "div" "mt-10 flex justify-center" $ do
    R.elClass "div" "flex flex-col gap-4 w-96" $ mdo
      
      fullNameInput <- R.elClass "div" "flex justify-between" $ do
        R.elClass "p" "font-normal text-brand-lg" $ R.text "Full name"
        Input.rawTextClass "border px-1"
      let fullName :: R.Dynamic t Text = R.value fullNameInput
        
      emailInput <- R.elClass "div" "flex justify-between" $ do
        R.elClass "p" "font-normal text-brand-lg" $ R.text "Email"
        Input.rawEmailClass "border px-1"
      let email :: R.Dynamic t Text = R.value emailInput
        
      passwordInput <- R.elClass "div" "flex justify-between" $ do
        R.elClass "p" "font-normal text-brand-lg" $ R.text "Password"
        Input.rawPasswordClass "border px-1"
      let password :: R.Dynamic t Text = R.value passwordInput

      confirmPasswordInput <- R.elClass "div" "flex justify-between" $ do
        R.elClass "p" "font-normal text-brand-lg" $ R.text "Confirm password"
        Input.rawPasswordClass "border px-1"
      let confirmPassword :: R.Dynamic t Text = R.value confirmPasswordInput
      
      R.dyn_ passwordMatchError
      R.dyn_ passwordEmptyError
        
      registerButton :: R.Event t () <- Button.primary' "Create my account"

      spinner <- Spinner.holdSmall triggerRequest
      status <- R.holdDyn R.blank . R.leftmost . map R.updated $ [spinner, message]
      R.dyn_ status

      let register = R.leftmost
            [ registerButton
            , R.keydown Key.Enter fullNameInput
            , R.keydown Key.Enter emailInput
            , R.keydown Key.Enter passwordInput
            , R.keydown Key.Enter confirmPasswordInput
            ]

      let passwordMatch :: R.Dynamic t Bool = (==) <$> password <*> confirmPassword
      passwordMatchError <- R.holdDyn R.blank $ R.ffor (R.tagPromptlyDyn passwordMatch register) $ \case
        True -> R.blank
        False -> R.elClass "p" "text-red-500" $ R.text "Password must match"

      let passwordNotEmpty :: R.Dynamic t Bool = (/= "") <$> password
      passwordEmptyError <- R.holdDyn R.blank $ R.ffor (R.tagPromptlyDyn passwordNotEmpty register) $ \case
        True -> R.blank
        False -> R.elClass "p" "text-red-500" $ R.text "Password cannot be empty"

      let triggerRequest = () <$
            R.ffilter
            and
            (R.tagPromptlyDyn (R.distributeListOverDyn [passwordMatch, passwordNotEmpty]) register)

      response :: R.Event t (Either Error.Error ()) <- Api.postRequest
        ((,,) <$> fullName <*> email <*> password)
        register
        (Route.Api_Register :/ ())
        (\(fullName', email', password') ->
           Register.Register (CI.mk fullName') (CI.mk email') password')

      message :: R.Dynamic t (m ()) <- R.holdDyn R.blank
        $ R.ffor response $ \case
        Left e -> do
          R.elClass "p" "text-red-500" $ R.text (Error.message e)
        Right _ -> do
          R.elClass "p" "text-green-600" $ R.text
            "Almost done... We'll send you an email in 5 minutes. Open it up to activate your account."
            
      return ()
