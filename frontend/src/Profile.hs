module Profile
  ( widget
  ) where

import Common.Lib.Prelude

import qualified Language.Javascript.JSaddle as JS
import qualified Reflex.Dom.Core as R
import qualified Obelisk.Route.Frontend as Ob

import qualified Common.Route as Route
import qualified Common.Api.ChangePassword as ChangePassword
import qualified Common.Api.Error as Error
import qualified Widget.Button as Button
import qualified Widget.Input as Input

widget
  :: forall t m js.
     ( R.DomBuilder t m
     , Ob.SetRoute t (Ob.R Route.FrontendRoute) m
     , Ob.RouteToUrl (Ob.R Route.FrontendRoute) m
     , R.Prerender js t m
     , R.PerformEvent t m
     , R.MonadHold t m
     , R.PostBuild t m
     , MonadFix m
     , R.TriggerEvent t m
     , R.HasJSContext (R.Performable m)
     , JS.MonadJSM (R.Performable m)
     )
  => m ()
widget = do
  R.elClass "div" "flex flex-col items-center mx-20" $ do
    R.elClass "div" "w-full" $ do
      
      section "Change password" $ do
        R.elClass "div" "flex flex-col gap-4 w-96" $ mdo
          oldPassword :: R.Dynamic t Text <- R.elClass "div" "flex justify-between" $ do
            R.elClass "p" "" $ R.text "Old password"
            Input.passwordClass "border px-1"
          R.dyn_ changePasswordError
          newPassword :: R.Dynamic t Text <- R.elClass "div" "flex justify-between" $ do
            R.elClass "p" "" $ R.text "New password"
            Input.passwordClass "border px-1"
          confirmNewPassword :: R.Dynamic t Text <- R.elClass "div" "flex justify-between" $ do
            R.elClass "p" "" $ R.text "Confirm new password"
            Input.passwordClass "border px-1"
          R.dyn_ passwordMatchError
          R.dyn_ passwordEmptyError
          changePassword :: R.Event t () <- Button.primary' "Save"
          R.dyn_ changePasswordSuccess

          let passwordMatch :: R.Dynamic t Bool = (==) <$> newPassword <*> confirmNewPassword
          passwordMatchError <- R.holdDyn R.blank $ R.ffor (R.tagPromptlyDyn passwordMatch changePassword) $ \case
            True -> R.blank
            False -> R.elClass "p" "text-red-500" $ R.text "Password must match"

          let passwordNotEmpty :: R.Dynamic t Bool = (\x -> x /= "") <$> newPassword
          passwordEmptyError <- R.holdDyn R.blank $ R.ffor (R.tagPromptlyDyn passwordNotEmpty changePassword) $ \case
            True -> R.blank
            False -> R.elClass "p" "text-red-500" $ R.text "Password cannot be empty"
            
          changePasswordResponse <- changePasswordAttempt oldPassword newPassword
            $ R.gate (R.current $ (&&) <$> passwordMatch <*> passwordNotEmpty) changePassword
          changePasswordError <- R.holdDyn R.blank $ R.ffor changePasswordResponse $ \case
            Nothing -> R.blank
            Just e -> R.elClass "p" "text-red-500" $ R.text (Error.message e)
          changePasswordSuccess <- R.holdDyn R.blank $ R.ffor changePasswordResponse $ \case
            Just _ -> R.blank
            Nothing -> R.elClass "p" "text-green-600" $ R.text "Your password has been changed"
          
          return ()
          
      section "Sign out" $ do
        Ob.routeLink (Route.FrontendRoute_SignOut :/ ()) $ do
          Button.secondary "Sign out"

  where
    section txt body = do
      R.elClass "div" "mt-10" $ do
        R.elClass "p" "text-brand-lg border-b border-brand-black mb-4" $ R.text txt
        body

    changePasswordAttempt
      :: R.Dynamic t Text -- ^ Old password
      -> R.Dynamic t Text -- ^ New password
      -> R.Event t () -- ^ Event to trigger request
      -> m (R.Event t (Maybe Error.Error))
    changePasswordAttempt oldPassword newPassword trigger = do
      let ev :: R.Event t (Text, Text) = R.tagPromptlyDyn (R.zipDyn oldPassword newPassword) trigger
      r <- R.performRequestAsync $ R.ffor ev $ \(oldPassword', newPassword') -> do
        let url = Route.apiHref $ Route.Api_ChangePassword :/ ()
        let body = ChangePassword.ChangePassword (ChangePassword.OldPassword oldPassword') newPassword'
        R.postJson url body
      return $ R.decodeXhrResponse <$> r
