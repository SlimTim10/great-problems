module SignIn
  ( widget
  ) where

import qualified Language.Javascript.JSaddle as JS
import qualified Data.Aeson as JSON
import qualified Data.CaseInsensitive as CI
import qualified Reflex.Dom.Core as R

import qualified Common.Route as Route
import qualified Inputs
import qualified Buttons
import qualified Util
import qualified Common.Api.Auth as Auth
import Global

widget
  :: forall t m.
     ( R.DomBuilder t m
     , R.PerformEvent t m
     , JS.MonadJSM (R.Performable m)
     , R.HasJSContext (R.Performable m)
     , R.TriggerEvent t m
     )
  => m ()
widget = do
  R.elClass "div" "mt-10 flex justify-center" $ do
    R.elClass "div" "flex flex-col gap-4 w-80" $ do
      email :: R.Dynamic t Text <- R.elClass "div" "flex justify-between" $ do
        R.elClass "p" "font-normal text-brand-lg" $ R.text "Email"
        email <- Inputs.emailClass "border px-1"
        return email
      password :: R.Dynamic t Text <- R.elClass "div" "flex justify-between" $ do
        R.elClass "p" "font-normal text-brand-lg" $ R.text "Password"
        password <- Inputs.passwordClass "border px-1"
        return password
      signIn :: R.Event t () <- Buttons.primary' "Sign in"
      let e :: R.Event t (Text, Text) = R.tagPromptlyDyn (R.zipDyn email password) signIn
      mResponse :: R.Event t (Maybe Text) <-
        fmap R.decodeXhrResponse
        <$> (R.performRequestAsync $ (\(email', password') -> signInRequest (Auth.Auth (CI.mk email') password')) <$> e)
      void $ R.performEvent $ R.ffor mResponse $ \mx -> JS.liftJSM $ do
        case mx of
          Just x -> Util.consoleLog x
          Nothing -> return ()
      R.blank

signInRequest :: JSON.ToJSON a => a -> R.XhrRequest Text
signInRequest body = R.postJson url body
  where
    url = Route.apiHref (Route.Api_SignIn :/ ())
