{-# LANGUAGE PackageImports #-}
module Header
  ( widget
  ) where

import qualified Obelisk.Route.Frontend as Ob
import qualified Reflex.Dom.Core as R
import qualified MyReflex.Dom.Widget.Basic as R'
import qualified "jsaddle-dom" GHCJS.DOM.Document as DOM
import qualified Language.Javascript.JSaddle as JS
import qualified Data.CaseInsensitive as CI

import qualified Common.Route as Route
import qualified Buttons as Buttons
import qualified Common.Api.User as User
import qualified Util
import Global

widget
  :: forall t m js.
     ( R.DomBuilder t m
     , Ob.SetRoute t (Ob.R Route.FrontendRoute) m
     , Ob.RouteToUrl (Ob.R Route.FrontendRoute) m
     , R.Prerender js t m
     , R.HasDocument m
     , DOM.IsDocument (R.RawDocument (R.DomBuilderSpace m))
     , JS.MonadJSM m
     )
  => m ()
widget = R.elClass "header" "h-14 py-2 px-3 flex items-center justify-between border-b" $ do
  Ob.routeLink (Route.FrontendRoute_Home :/ ()) $ do
    R.elClass "p" "font-medium text-xl"
      $ R.text "Great Problems"
  Ob.routeLink (Route.FrontendRoute_Explore :/ Nothing) $ do
    Buttons.secondary "Explore"
  R'.elAttrClass
    "input"
    ("type" =: "search" <> "placeholder" =: "Search...")
    "border rounded h-8 w-1/2 px-1"
    $ R.blank
  R.el "div" $ do
    Util.getCurrentUser >>= \case
      Just user -> do
        R.el "p" $ R.text $ CI.original $ User.full_name user
      Nothing -> do
        R.elClass "span" "pr-2" $ do
          Ob.routeLink (Route.FrontendRoute_Register :/ ()) $ do
            Buttons.primary "Create an account"
        Ob.routeLink (Route.FrontendRoute_SignIn :/ ()) $ do
          Buttons.secondary "Sign in"
