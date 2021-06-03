module Home
  ( widget
  ) where

import qualified Control.Monad.Fix as Fix
import qualified Language.Javascript.JSaddle as JS
import qualified Obelisk.Route.Frontend as Ob
import qualified Reflex.Dom.Core as R

import qualified Common.Route as Route
import qualified Common.Api.Problem as Problem
import Global

widget
  :: forall t m js.
     ( R.DomBuilder t m
     , R.PostBuild t m
     , R.MonadHold t m
     , Fix.MonadFix m
     , JS.MonadJSM m
     , JS.MonadJSM (R.Performable m)
     , R.HasJSContext (R.Performable m)
     , R.PerformEvent t m
     , R.TriggerEvent t m
     , Ob.RouteToUrl (Ob.R Route.FrontendRoute) m
     , Ob.SetRoute t (Ob.R Route.FrontendRoute) m
     , R.Prerender js t m
     )
  => m ()
widget = do
  R.elClass "div" "flex flex-col items-center" $ do
    onload <- R.getPostBuild
    let endpoint :: R.Event t Text = R.tag (R.current (R.constDyn "/api/problems")) onload
    response :: R.Event t (Maybe [Problem.Problem]) <- R.getAndDecode endpoint
    problems :: R.Dynamic t [Problem.Problem] <- R.holdDyn [] $ fromMaybe [] <$> response
    void $ R.simpleList problems problemItem

problemItem
  :: ( R.DomBuilder t m
     , R.PostBuild t m
     , R.MonadSample t m
     , Ob.RouteToUrl (Ob.R Route.FrontendRoute) m
     , Ob.SetRoute t (Ob.R Route.FrontendRoute) m
     , R.Prerender js t m
     )
  =>  R.Dynamic t Problem.Problem
  -> m ()
problemItem problem = R.elClass "div" "border" $ do
  R.elClass "p" "" $ do
    R.dynText $ "Title: " <> (Problem.title <$> problem)
  void $ R.dyn $ R.ffor (Problem.description <$> problem) $ \case
    Nothing -> R.blank
    Just description -> R.elClass "p" "" $ do
      R.text $ "Description: " <> description
  R.elDynAttr "img"
    (("src" =:) <$> Problem.thumnail_url <$> problem)
    R.blank
  problemId <- R.sample . R.current $ Problem.id <$> problem
  Ob.routeLink (Route.FrontendRoute_ViewProblem :/ problemId) $ do
    R.elClass "p" "" $ R.text "View"
