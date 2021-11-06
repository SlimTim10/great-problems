module Tabs
  ( widget
  , Tab(..)
  ) where

import Common.Lib.Prelude

import qualified Obelisk.Route.Frontend as Ob
import qualified Reflex.Dom.Core as R

import qualified Common.Route as Route

data Tab = Problems | ProblemSets
  deriving (Eq)

widget
  :: forall t m js.
     ( R.DomBuilder t m
     , Ob.SetRoute t (Ob.R Route.FrontendRoute) m
     , Ob.RouteToUrl (Ob.R Route.FrontendRoute) m
     , R.Prerender js t m
     )
  => Tab
  -> m ()
widget activeTab = do
  R.elClass "div" "my-6 flex justify-center" $ do
    R.elClass "div" "flex justify-around w-brand-screen-lg" $ do
      tabWidget (activeTab == Problems)
        (Ob.routeLink (Route.FrontendRoute_Explore :/ (Just (Route.ExploreRoute_Problems :/ ()))))
        "Problems"
      tabWidget (activeTab == ProblemSets)
        (Ob.routeLink (Route.FrontendRoute_Explore :/ (Just (Route.ExploreRoute_ProblemSets :/ ()))))
        "Problem sets"
  where
    tabWidget active routeLink txt = case active of
      True -> do
        R.elClass "div" "flex-1 border-b-2 border-brand-black" $ do
          routeLink $ do
            R.elClass "p" "text-center text-brand-lg font-normal" $ R.text txt
      False -> do
        R.elClass "div" "flex-1 border-b border-light-gray" $ do
          routeLink $ do
            R.elClass "p" "text-center text-brand-lg text-brand-light-gray font-light" $ R.text txt
