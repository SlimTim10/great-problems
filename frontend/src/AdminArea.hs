module AdminArea
  ( widget
  ) where

import Common.Lib.Prelude
import qualified Frontend.Lib.Util as Util

import qualified Data.Map as Map
import qualified Data.CaseInsensitive as CI
import qualified Language.Javascript.JSaddle as JS
import qualified Obelisk.Route.Frontend as Ob
import qualified Obelisk.Generated.Static as Ob
import qualified Reflex.Dom.Core as R

import qualified Common.Route as Route
import qualified Common.Api.User as User
import qualified Common.Api.Role as Role
import qualified Common.Api.Error as Error
import qualified Widget.Input as Input
-- import qualified Widget.Button as Button

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
  R.elClass "div" "flex justify-center" $ do
    R.elClass "div" "w-brand-screen-lg flex flex-col gap-10" $ do
      R.elClass "div" "" $ do
        R.elClass "p" "text-center text-brand-lg font-normal" $ R.text "Requests"
        R.elClass "p" "font-normal" $ R.text "0 requests"
      R.elClass "div" "" $ do
        R.elClass "p" "text-center text-brand-lg font-normal" $ R.text "Users"
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
              (Route.FrontendRoute_ViewUser :/ (User.id u)) $ do
              R.text $ CI.original $ User.fullName u
          selectedRole :: R.Dynamic t Role.Role <- R.elClass "div" ""
            $ selectRoleWidget (User.role u)
            
          response <- updateRoleAttempt u selectedRole
          let updateRoleError :: R.Event t (Either Text ()) = maybeToEither Error.message <$> response

          spinner <- R.holdDyn R.blank $ R.ffor (R.updated selectedRole) . const
            $ R.elAttr "img" ("src" =: Ob.static @"small_spinner.svg" <> "alt" =: "loading") $ R.blank
          responseEl <- R.holdDyn R.blank . R.ffor updateRoleError $ \case
            Left e -> R.elClass "p" "text-red-500" $ R.text e
            Right _ -> R.elClass "p" "text-green-500" $ R.text "Success"
          status <- R.holdDyn R.blank . R.leftmost . map R.updated $ [spinner, responseEl]
          R.dyn_ status

          R.blank

    selectRoleWidget
      :: Role.Role -- ^ Initial value
      -> m (R.Dynamic t Role.Role)
    selectRoleWidget initialValue = do
      let roles :: [Role.Role] = [minBound ..]
      let roleNames :: [Text] = map (cs . show) roles
      let roleList :: [(Role.Role, Text)] = zip roles roleNames
      let dropdownItems = R.constDyn $ Map.fromList roleList
      Input.dropdownClass' "border border-brand-light-gray w-full" initialValue dropdownItems R.never

    updateRoleAttempt :: User.User -> R.Dynamic t Role.Role -> m (R.Event t (Maybe Error.Error))
    updateRoleAttempt user selectedRole = do
      r <- R.performRequestAsync $ R.ffor (R.updated selectedRole) $ \role -> do
        let url = Route.apiHref $ Route.Api_Users :/ (Just $ User.id user)
        R.postJson url (User.UpdateRequest role)
      return $ R.decodeXhrResponse <$> r
