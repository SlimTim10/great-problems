module Register
  ( widget
  ) where

import qualified Obelisk.Route.Frontend as Ob
import qualified Reflex.Dom.Core as R
import qualified MyReflex.Dom.Widget.Basic as R'

import qualified Common.Route as Route
import qualified Buttons as Buttons

import Global

widget
  :: forall t m js.
     ( R.DomBuilder t m
     , Ob.SetRoute t (Ob.R Route.FrontendRoute) m
     , Ob.RouteToUrl (Ob.R Route.FrontendRoute) m
     , R.Prerender js t m
     )
  => m ()
widget = do
  R.elClass "div" "mt-10 flex justify-center" $ do
    R.elClass "div" "flex flex-col gap-4 w-80" $ do
      R.elClass "div" "flex justify-between" $ do
        R.elClass "p" "font-normal text-brand-lg" $ R.text "Full name"
        R'.elAttrClass
          "input"
          ("type" =: "text")
          "border px-1"
          $ R.blank
      R.elClass "div" "flex justify-between" $ do
        R.elClass "p" "font-normal text-brand-lg" $ R.text "Email"
        R'.elAttrClass
          "input"
          ("type" =: "email")
          "border px-1"
          $ R.blank
      R.elClass "div" "flex justify-between" $ do
        R.elClass "p" "font-normal text-brand-lg" $ R.text "Password"
        R'.elAttrClass
          "input"
          ("type" =: "password")
          "border px-1"
          $ R.blank
      Buttons.primary "Create my account"
