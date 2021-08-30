module SignOut
  ( widget
  ) where

import qualified Language.Javascript.JSaddle as JS
import qualified Reflex.Dom.Core as R
import qualified Obelisk.Route.Frontend as Ob

import qualified Common.Route as Route
import qualified Common.Api.Error as Error
import Global

widget
  :: forall t m.
     ( R.DomBuilder t m
     , R.PerformEvent t m
     , JS.MonadJSM (R.Performable m)
     , R.HasJSContext (R.Performable m)
     , R.TriggerEvent t m
     , Ob.SetRoute t (Ob.R Route.FrontendRoute) m
     , R.PostBuild t m
     )
  => m ()
widget = do
  pb <- R.getPostBuild
  let url = Route.apiHref (Route.Api_SignOut :/ ())
  signOut :: R.Event t (Maybe Error.Error) <- do
    r <- R.performRequestAsync $ (const $ R.postJson url ()) <$> pb
    return $ R.decodeXhrResponse <$> r
  Ob.setRoute $ Route.FrontendRoute_Explore :/ Nothing <$ signOut
