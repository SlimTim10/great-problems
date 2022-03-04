module Frontend.Lib.Api where

import Common.Lib.Prelude

import qualified Data.Aeson as JSON
import qualified Language.Javascript.JSaddle as JS
import qualified Reflex.Dom.Core as R
import qualified Obelisk.Route.Frontend as Ob

import qualified Common.Route as Route
import qualified Common.Api.Error as Error

request
  :: forall t m a b c.
     ( JS.MonadJSM (R.Performable m)
     , R.HasJSContext (R.Performable m)
     , R.PerformEvent t m
     , R.TriggerEvent t m
     , JSON.ToJSON b
     , JSON.FromJSON c
     )
  => R.Dynamic t a -- ^ Payload
  -> R.Event t () -- ^ Event to trigger request
  -> Ob.R Route.Api -- ^ API route
  -> (a -> b) -- ^ Prepare the request body as JSON
  -> m (R.Event t (Either Error.Error c))
request payload trg url jsonBody = do
  let ev = R.tagPromptlyDyn payload trg
  r :: R.Event t R.XhrResponse <- R.performRequestAsync
    $ R.postJson (Route.apiHref url) <$> jsonBody <$> ev

  let err :: R.Event t (Maybe Error.Error) = R.decodeXhrResponse <$> r
  let retErr :: R.Event t (Either Error.Error c) = Left . fromJust
        <$> R.ffilter isJust err

  let success :: R.Event t (Maybe c) = R.decodeXhrResponse <$> r
  let retSuccess :: R.Event t (Either Error.Error c) = Right . fromJust
        <$> R.ffilter isJust success

  return $ R.leftmost [retErr, retSuccess]
