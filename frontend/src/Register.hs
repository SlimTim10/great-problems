module Register
  ( widget
  ) where

import Common.Lib.Prelude

import qualified Language.Javascript.JSaddle as JS
import qualified Data.Aeson as JSON
import qualified Data.CaseInsensitive as CI
import qualified Reflex.Dom.Core as R

import qualified Common.Route as Route
import qualified Widget.Input as Input
import qualified Widget.Button as Button
import qualified Common.Api.Error as Error
import qualified Common.Api.Register as Register

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
    R.elClass "div" "flex flex-col gap-4 w-80" $ do
      fullName :: R.Dynamic t Text <- R.elClass "div" "flex justify-between" $ do
        R.elClass "p" "font-normal text-brand-lg" $ R.text "Full name"
        Input.textClass "border px-1"
      email :: R.Dynamic t Text <- R.elClass "div" "flex justify-between" $ do
        R.elClass "p" "font-normal text-brand-lg" $ R.text "Email"
        Input.emailClass "border px-1"
      password :: R.Dynamic t Text <- R.elClass "div" "flex justify-between" $ do
        R.elClass "p" "font-normal text-brand-lg" $ R.text "Password"
        Input.passwordClass "border px-1"
      register :: R.Event t () <- Button.primary' "Create my account"

      response <- registerAttempt fullName email password register
      
      registerError :: R.Event t (Either Text ()) <- R.performEvent $ R.ffor response $ \case
        Just e -> return $ Left (Error.message e)
        Nothing -> return $ Right ()
      registerErrorText :: R.Dynamic t Text <- R.holdDyn "" $ fromLeft "" <$> registerError
      R.elClass "p" "text-red-500" $ R.dynText registerErrorText

      registerSuccess :: R.Event t (Either (R.Dynamic t Text) (R.Dynamic t ())) <- fmap R.updated
        $ R.eitherDyn =<< R.holdDyn (Left mempty) registerError
      registerSuccessText :: R.Dynamic t Text <- R.holdDyn ""
        $ "Almost done... We'll send you an email in 5 minutes. Open it up to activate your account."
        <$ registerSuccess
      R.elClass "p" "text-green-600" $ R.dynText registerSuccessText
  where
    registerAttempt
      :: R.Dynamic t Text
      -> R.Dynamic t Text
      -> R.Dynamic t Text
      -> R.Event t ()
      -> m (R.Event t (Maybe Error.Error))
    registerAttempt fullName email password register = do
      let ev :: R.Event t (Text, Text, Text) = R.tagPromptlyDyn
            (R.distributeListOverDynWith (\xs -> (xs !! 0, xs !! 1, xs !! 2)) [fullName, email, password])
            register
      r <- R.performRequestAsync
        $ (\(fullName', email', password') -> registerRequest
            $ Register.Register (CI.mk fullName') (CI.mk email') password')
        <$> ev
      return $ R.decodeXhrResponse <$> r

    registerRequest :: JSON.ToJSON a => a -> R.XhrRequest Text
    registerRequest body = R.postJson url body
      where url = Route.apiHref (Route.Api_Register :/ ())
