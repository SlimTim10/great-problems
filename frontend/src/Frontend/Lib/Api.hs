module Frontend.Lib.Api where

import qualified Data.Aeson as JSON
import qualified Language.Javascript.JSaddle as JS
import qualified Reflex.Dom.Core as R
import qualified Obelisk.Route.Frontend as Ob

import qualified Common.Route as Route
import qualified Common.Api.Error as Error

data Response t a = Response
  { resError :: R.Event t (Maybe Error.Error)
  , resSuccess :: R.Event t (Maybe a)
  }

request
  :: ( JS.MonadJSM (R.Performable m)
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
  -> m (Response t c)
request payload trg url jsonBody = do
  let ev = R.tagPromptlyDyn payload trg
  r :: R.Event t R.XhrResponse <- R.performRequestAsync $ R.postJson (Route.apiHref url) <$> jsonBody <$> ev
  return $ Response (R.decodeXhrResponse <$> r) (R.decodeXhrResponse <$> r)
