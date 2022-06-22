module Problem.Duplicate
  ( widget
  ) where

import Common.Lib.Prelude

import qualified Language.Javascript.JSaddle as JS
import qualified Reflex.Dom.Core as R

import qualified Common.Route as Route
import qualified Common.Api.Error as Error
import qualified Common.Api.Problem as Problem
import qualified Frontend.Lib.Api as Api
import qualified Frontend.Lib.Util as Util

widget
  :: ( R.DomBuilder t m
     , R.PostBuild t m
     , JS.MonadJSM (R.Performable m)
     , R.HasJSContext (R.Performable m)
     , R.PerformEvent t m
     , R.TriggerEvent t m
     , R.MonadHold t m
     )
  => Integer
  -> m ()
widget problemId = do
  onload <- R.getPostBuild
  response :: R.Event t (Either Error.Error Problem.Problem) <- Api.postRequest
    (R.constDyn ())
    onload
    (Route.Api_DuplicateProblem :/ problemId)
    (const ())

  errorMessage :: R.Dynamic t (m ()) <- R.holdDyn R.blank
    $ R.ffor (R.filterLeft response)
    $ \e -> do
    R.elClass "p" "text-red-500" $ R.text (Error.message e)
    
  R.dyn_ errorMessage
  Util.redirectWithoutHistory
    $ (\p -> Route.FrontendRoute_Problems :/ (Problem.id p, Route.ProblemsRoute_Edit :/ ()))
    <$> R.filterRight response
