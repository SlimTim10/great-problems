module AdminDashboard.Users
  ( widget
  ) where

import qualified Data.Map as Map
import qualified Data.CaseInsensitive as CI
import qualified Language.Javascript.JSaddle as JS
import qualified Obelisk.Route.Frontend as Ob
import qualified Reflex.Dom.Core as R

import Common.Lib.Prelude
import qualified AdminDashboard.Util as DashUtil
import qualified Common.Api.User as User
import qualified Frontend.Lib.Util as Util
import qualified Common.Route as Route
import qualified Common.Api.Role as Role
import qualified Common.Api.Error as Error
import qualified Frontend.Lib.Api as Api
import qualified Widget.Spinner as Spinner
import qualified Widget.Input as Input

widget
  :: forall t m js.
     ( R.DomBuilder t m
     , R.PostBuild t m
     , R.MonadHold t m
     , MonadFix m
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
  DashUtil.title "Users"
  response :: R.Event t (Maybe [User.User]) <- Util.getOnload
    $ Route.apiHref $ Route.Api_Users :/ Nothing
  users :: R.Dynamic t [User.User] <- R.holdDyn [] $ fromMaybe [] <$> response
  R.elClass "ul" "flex flex-col gap-4" $ do
    void $ R.simpleList users viewUser
  where
    viewUser :: R.Dynamic t User.User -> m ()
    viewUser user = do
      R.elClass "li" "grid grid-cols-3 gap-4" $ do
        Util.dynFor user $ \u -> do
          R.elClass "p" "" $ do
            Ob.routeLink
              (Route.FrontendRoute_Profile :/ (User.id u)) $ do
              R.text $ CI.original $ User.fullName u
          selectedRole :: R.Dynamic t Role.Role <- R.elClass "div" ""
            $ selectRoleWidget (User.role u)
            
          response :: R.Event t (Either Error.Error ()) <- Api.postRequest
            selectedRole
            (() <$ R.updated selectedRole)
            (Route.Api_Users :/ (Just $ User.id u))
            (\role -> User.UpdateRequest role)

          spinner <- Spinner.holdSmall (R.updated selectedRole)
          message :: R.Dynamic t (m ()) <- R.holdDyn R.blank
            $ R.ffor response
            $ \case
            Left e -> do
              R.elClass "p" "text-red-500" $ R.text (Error.message e)
            Right _ -> do
              R.elClass "p" "text-green-500" $ R.text "Success"
          status <- R.holdDyn R.blank . R.leftmost . map R.updated $ [spinner, message]
          R.dyn_ status

    selectRoleWidget
      :: Role.Role -- ^ Initial value
      -> m (R.Dynamic t Role.Role)
    selectRoleWidget initialValue = do
      let roles :: [Role.Role] = [minBound ..]
      let roleNames :: [Text] = map (cs . show) roles
      let roleList :: [(Role.Role, Text)] = zip roles roleNames
      let dropdownItems = R.constDyn $ Map.fromList roleList
      Input.dropdownClass' "border border-brand-light-gray w-full" initialValue dropdownItems R.never
