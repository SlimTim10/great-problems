{-# LANGUAGE PackageImports #-}
module Header
  ( widget
  ) where

import qualified Web.KeyCode as Key
import qualified Obelisk.Route.Frontend as Ob
import qualified Reflex.Dom.Core as R
import qualified "ghcjs-dom" GHCJS.DOM.Document as DOM
import qualified Language.Javascript.JSaddle as JS
import qualified Data.CaseInsensitive as CI

import Common.Lib.Prelude
import qualified Frontend.Lib.Util as Util
import qualified Common.Route as Route
import qualified Widget.Button as Button
import qualified Common.Api.User as User
import qualified Common.Api.Role as Role
import qualified Common.Api.Search as Search

widget
  :: forall t m js.
     ( R.DomBuilder t m
     , Ob.SetRoute t (Ob.R Route.FrontendRoute) m
     , Ob.RouteToUrl (Ob.R Route.FrontendRoute) m
     , R.Prerender js t m
     , R.HasDocument m
     , DOM.IsDocument (R.RawDocument (R.DomBuilderSpace m))
     , JS.MonadJSM m
     , R.PostBuild t m
     )
  => m ()
widget = R.elClass "header" "h-14 py-2 px-3 flex items-center justify-between border-b border-brand-light-gray" $ do
  Ob.routeLink (Route.FrontendRoute_Home :/ ()) $ do
    R.elClass "p" "font-medium text-xl"
      $ R.text "Great Problems"
  Ob.routeLink (Route.FrontendRoute_Search :/ mempty) $ do
    Button.primary "Explore"

  -- Search bar
  searchTermInput <- R.inputElement
    $ R.def & R.inputElementConfig_elementConfig . R.elementConfig_initialAttributes .~
    ( "type" =: "search"
      <> "placeholder" =: "Search..."
      <> "class" =: "border rounded h-8 w-1/2 px-1"
    )
  let searchTerm :: R.Dynamic t (Maybe Text) = fmap textToMaybe . R.value $ searchTermInput
  let params :: R.Dynamic t Search.Params = Search.Params
        <$> searchTerm
        <*> R.constDyn Nothing
        <*> R.constDyn Nothing
        <*> R.constDyn Nothing
  Util.dynFor params $ \params' ->
    Ob.setRoute $ (Route.FrontendRoute_Search :/ Search.paramsToQuery params')
    <$ R.keydown Key.Enter searchTermInput
    
  Util.getCurrentUser >>= \case
    Just user -> do
      when (User.role user == Role.Basic) $ do
        Ob.routeLink (Route.FrontendRoute_NewProblem :/ ()) $ do
          Button.primary "Try the editor!"
      R.elClass "div" "flex items-center" $ do
        when (User.role user == Role.Administrator) $ do
          R.elClass "span" "mr-2" $ do
            Ob.routeLink (Route.FrontendRoute_Admin :/ ()) $ do
              Button.primary "Admin"
        when (User.role user `elem` [Role.Contributor, Role.Moderator, Role.Administrator]) $ do
          R.elClass "span" "mr-2" $ do
            Ob.routeLink (Route.FrontendRoute_NewProblem :/ ()) $ do
              Button.primary "Create"
        Ob.routeLink (Route.FrontendRoute_Settings :/ ()) $ do
          R.elClass "p" "font-medium mr-2" $ R.text $ CI.original $ User.fullName user
    Nothing -> do
      Ob.routeLink (Route.FrontendRoute_NewProblem :/ ()) $ do
        Button.primary "Try the editor!"
      R.elClass "div" "flex items-center" $ do
        R.elClass "span" "mr-2" $ do
          Ob.routeLink (Route.FrontendRoute_SignIn :/ ()) $ do
            Button.secondary "Sign in"
        Ob.routeLink (Route.FrontendRoute_Register :/ ()) $ do
          Button.primary "Create an account"
