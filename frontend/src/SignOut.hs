module SignOut
  ( widget
  ) where

import Common.Lib.Prelude

import qualified Language.Javascript.JSaddle as JS
import qualified Reflex.Dom.Core as R
import qualified Obelisk.Route.Frontend as Ob

import qualified Common.Route as Route
import qualified Common.Api.Error as Error
import qualified Frontend.Lib.Api as Api

widget
  :: forall t m.
     ( R.DomBuilder t m
     , R.PerformEvent t m
     , JS.MonadJSM (R.Performable m)
     , R.HasJSContext (R.Performable m)
     , R.TriggerEvent t m
     , Ob.SetRoute t (Ob.R Route.FrontendRoute) m
     , R.PostBuild t m
     , R.MonadHold t m
     )
  => m ()
widget = do
  onload <- R.getPostBuild
  
  response :: R.Event t (Either Error.Error ()) <- Api.postRequest
    (R.constDyn ())
    onload
    (Route.Api_SignOut :/ ())
    id
    
  errorMessage :: R.Dynamic t (m ()) <- R.holdDyn R.blank
    $ R.ffor (R.filterLeft response)
    $ \e -> do
    R.elClass "p" "text-red-500" $ R.text (Error.message e)
        
  R.dyn_ errorMessage
  Ob.setRoute $ Route.FrontendRoute_Home :/ () <$ R.filterRight response
