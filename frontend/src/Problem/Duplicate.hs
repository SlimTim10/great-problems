module Problem.Duplicate
  ( widget
  ) where

import Common.Lib.Prelude

import qualified Language.Javascript.JSaddle as JS
import qualified Reflex.Dom.Core as R
import qualified Obelisk.Route.Frontend as Ob

import qualified Common.Route as Route
import qualified Common.Api.Error as Error
import qualified Common.Api.Problem as Problem
import qualified Frontend.Lib.Api as Api

widget
  :: ( R.DomBuilder t m
     , R.PostBuild t m
     , JS.MonadJSM (R.Performable m)
     , R.HasJSContext (R.Performable m)
     , R.PerformEvent t m
     , R.TriggerEvent t m
     , Ob.SetRoute t (Ob.R Route.FrontendRoute) m
     , MonadFix m
     , R.MonadHold t m
     )
  => Integer
  -> m ()
widget problemId = do
  onload <- R.getPostBuild
  response :: Api.Response t Problem.Problem <- Api.request
    (R.constDyn ())
    onload
    (Route.Api_DuplicateProblem :/ problemId)
    (const ())

  duplicateErrorText :: R.Dynamic t Text <- R.holdDyn "" $
    maybe "" Error.message <$> Api.resError response
  R.elClass "p" "text-red-500" $ R.dynText duplicateErrorText
  
  duplicateSuccess :: R.Event t (Maybe Problem.Problem) <- fmap R.updated
    $ R.improvingMaybe =<< R.holdDyn Nothing (Api.resSuccess response)
  Ob.setRoute $
    (\p -> Route.FrontendRoute_Problems :/ (Problem.id . fromJust $ p, Route.ProblemsRoute_Edit :/ ()))
    <$> duplicateSuccess
