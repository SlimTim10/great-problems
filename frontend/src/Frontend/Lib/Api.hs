module Frontend.Lib.Api where

import Common.Lib.Prelude

import qualified Data.Aeson as JSON
import qualified Language.Javascript.JSaddle as JS
import qualified Reflex.Dom.Core as R
import qualified Obelisk.Route.Frontend as Ob

import qualified Common.Route as Route
import qualified Common.Api.Error as Error

data Method = GET | DELETE
  deriving (Show)

postRequest
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
postRequest payload trg url jsonBody = do
  let ev = R.tagPromptlyDyn payload trg
  res :: R.Event t R.XhrResponse <- R.performRequestAsync
    $ R.postJson (Route.apiHref url) <$> jsonBody <$> ev

  let err :: R.Event t (Maybe Error.Error) = R.decodeXhrResponse <$> res
  let resErr :: R.Event t (Either Error.Error c) = Left . fromJust
        <$> R.ffilter isJust err

  let success :: R.Event t (Maybe c) = R.decodeXhrResponse <$> res
  let resSuccess :: R.Event t (Either Error.Error c) = Right . fromJust
        <$> R.ffilter isJust success

  return $ R.leftmost [resErr, resSuccess]

basicRequest
  :: forall t m a.
     ( JS.MonadJSM (R.Performable m)
     , R.HasJSContext (R.Performable m)
     , R.PerformEvent t m
     , R.TriggerEvent t m
     , JSON.FromJSON a
     )
  => Method
  -> R.Event t (Ob.R Route.Api) -- ^ API route
  -> m (R.Event t (Either Error.Error a))
basicRequest method url = do
  let textUrl :: R.Event t Text = Route.apiHref <$> url
  let req :: R.Event t (R.XhrRequest ()) =
        fmap
        (\x -> R.xhrRequest (cs . show $ method) x R.def)
        textUrl
  res :: R.Event t R.XhrResponse <- R.performRequestAsync req

  let err :: R.Event t (Maybe Error.Error) = R.decodeXhrResponse <$> res
  let resErr :: R.Event t (Either Error.Error a) = Left . fromJust
        <$> R.ffilter isJust err

  let success :: R.Event t (Maybe a) = R.decodeXhrResponse <$> res
  let resSuccess :: R.Event t (Either Error.Error a) = Right . fromJust
        <$> R.ffilter isJust success

  return $ R.leftmost [resErr, resSuccess]

getRequest
  :: forall t m a.
     ( JS.MonadJSM (R.Performable m)
     , R.HasJSContext (R.Performable m)
     , R.PerformEvent t m
     , R.TriggerEvent t m
     , JSON.FromJSON a
     )
  => R.Event t (Ob.R Route.Api) -- ^ API route
  -> m (R.Event t (Either Error.Error a))
getRequest = basicRequest GET

deleteRequest
  :: forall t m a.
     ( JS.MonadJSM (R.Performable m)
     , R.HasJSContext (R.Performable m)
     , R.PerformEvent t m
     , R.TriggerEvent t m
     , JSON.FromJSON a
     )
  => R.Event t (Ob.R Route.Api) -- ^ API route
  -> m (R.Event t (Either Error.Error a))
deleteRequest = basicRequest DELETE
