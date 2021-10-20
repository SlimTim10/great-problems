module SignIn
  ( widget
  ) where

import Frontend.Lib.Prelude

import qualified Language.Javascript.JSaddle as JS
import qualified Data.Aeson as JSON
import qualified Data.CaseInsensitive as CI
import qualified Reflex.Dom.Core as R
import qualified Obelisk.Route.Frontend as Ob

import qualified Common.Route as Route
import qualified Widget.Input as Input
import qualified Widget.Button as Button
import qualified Common.Api.Auth as Auth
import qualified Common.Api.Error as Error

widget
  :: forall t m.
     ( R.DomBuilder t m
     , R.PerformEvent t m
     , JS.MonadJSM (R.Performable m)
     , R.HasJSContext (R.Performable m)
     , R.TriggerEvent t m
     , Ob.SetRoute t (Ob.R Route.FrontendRoute) m
     , R.MonadHold t m
     , MonadFix m
     , R.PostBuild t m
     )
  => m ()
widget = do
  R.elClass "div" "mt-10 flex justify-center" $ do
    -- TODO: event should also be handled on pressing Enter
    R.elClass "div" "flex flex-col gap-4 w-80" $ do
      email :: R.Dynamic t Text <- R.elClass "div" "flex justify-between" $ do
        R.elClass "p" "font-normal text-brand-lg" $ R.text "Email"
        Input.emailClass "border px-1"
      password :: R.Dynamic t Text <- R.elClass "div" "flex justify-between" $ do
        R.elClass "p" "font-normal text-brand-lg" $ R.text "Password"
        Input.passwordClass "border px-1"
      signIn :: R.Event t () <- Button.primary' "Sign in"
      
      response <- signInAttempt email password signIn
      signInError :: R.Event t (Either Text ()) <- R.performEvent $ R.ffor response $ \case
        Just e -> return $ Left (Error.message e)
        Nothing -> return $ Right ()
        
      signInErrorText :: R.Dynamic t Text <- R.holdDyn "" $ fromLeft "" <$> signInError
      R.elClass "p" "text-red-500" $ R.dynText signInErrorText
      
      signInSuccess :: R.Event t (Either (R.Dynamic t Text) (R.Dynamic t ())) <- fmap R.updated
        $ R.eitherDyn =<< R.holdDyn (Left "") signInError
      Ob.setRoute $ Route.FrontendRoute_Explore :/ Nothing <$ signInSuccess
      
      R.blank
  where
    signInAttempt :: R.Dynamic t Text -> R.Dynamic t Text -> R.Event t () -> m (R.Event t (Maybe Error.Error))
    signInAttempt email password signIn = do
      let ev :: R.Event t (Text, Text) = R.tagPromptlyDyn (R.zipDyn email password) signIn
      r <- R.performRequestAsync
        $ (\(email', password') -> signInRequest $ Auth.Auth (CI.mk email') password')
        <$> ev
      return $ R.decodeXhrResponse <$> r

    signInRequest :: JSON.ToJSON a => a -> R.XhrRequest Text
    signInRequest body = R.postJson url body
      where url = Route.apiHref $ Route.Api_SignIn :/ ()
