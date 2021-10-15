module Home
  ( widget
  ) where

import qualified Control.Monad.Fix as Fix
import qualified Language.Javascript.JSaddle as JS
import qualified Obelisk.Route.Frontend as Ob
import qualified MyReflex.Dom.Widget.Basic as R'
import qualified Reflex.Dom.Core as R

import qualified Common.Route as Route
import qualified Common.Api.Problem as Problem
import qualified Widget.Button as Button
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
    R.elClass "div" "mt-2 mx-20" $ do
      R.elClass "p" "mr-10 text-brand-lg font-bold" $ R.text "The best practice problems & problem sets for any topic"
      R.elClass "p" "mt-2" $ R.text "Discover problems & problem sets in topics like Math, Electrical Engineering, Physics, and more, made by real teachers."
    R.elClass "div" "mt-10 flex flex-col items-center gap-2 w-full" $ do
      Ob.routeLink (Route.FrontendRoute_Explore :/ Nothing) $ do
        Button.primary "Explore"
      R.elClass "p" "" $ R.text "or"
      R.elClass "div" "flex justify-center w-full" $ do
        R'.elAttrClass
          "input"
          ("type" =: "search" <> "placeholder" =: "e.g. Newton's laws")
          "border rounded h-10 w-1/3 px-1 mr-1"
          $ R.blank
        Button.primary "Search"
