module AdminArea
  ( widget
  ) where

import Common.Lib.Prelude
import qualified Frontend.Lib.Util as Util

import qualified Data.Map as Map
import qualified Data.CaseInsensitive as CI
import qualified Language.Javascript.JSaddle as JS
import qualified Obelisk.Route.Frontend as Ob
import qualified Reflex.Dom.Core as R

import qualified Common.Route as Route
import qualified Common.Api.User as User
import qualified Common.Api.Role as Role
import qualified Common.Api.MetaSetting as MetaSetting
import qualified Common.Api.Error as Error
import qualified Frontend.Lib.Api as Api
import qualified Widget.Input as Input
import qualified Widget.Button as Button
import qualified Widget.Spinner as Spinner

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
      R.elClass "div" "" $ do
        R.elClass "p" "text-center text-brand-lg font-normal" $ R.text "Meta Settings"
        response :: R.Event t (Maybe [MetaSetting.MetaSetting]) <- Util.getOnload
          $ Route.apiHref $ Route.Api_MetaSettings :/ Nothing
        metaSettings :: R.Dynamic t [MetaSetting.MetaSetting] <- R.holdDyn [] $ fromMaybe [] <$> response
        R.elClass "ul" "flex flex-col gap-4" $ do
          void $ R.simpleList metaSettings viewMetaSetting
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

    viewMetaSetting :: R.Dynamic t MetaSetting.MetaSetting -> m ()
    viewMetaSetting metaSetting = do
      R.elClass "li" "grid grid-cols-3 gap-4" $ do
        Util.dynFor metaSetting $ \x -> mdo
          R.elClass "p" "justify-self-end self-center" $ R.text (cs . show $ MetaSetting.setting x)
          v :: R.Dynamic t Text <- fmap R.value $ R.inputElement $
            R.def
            & R.inputElementConfig_elementConfig . R.elementConfig_initialAttributes .~
            ( "type" =: "text"
              <> "class" =: "border px-1"
            )
            & R.inputElementConfig_initialValue .~ MetaSetting.value x

          save <- R.elClass "div" "flex gap-4" $ do
            save' :: R.Event t () <- Button.primarySmallClass' "Save" "w-16"
            R.dyn_ status
            return save'

          response :: R.Event t (Either Error.Error ()) <- Api.postRequest
            (R.zipDynWith MetaSetting.MetaSetting (R.constDyn $ MetaSetting.setting x) v)
            save
            (Route.Api_MetaSettings :/ Nothing)
            id

          spinner <- Spinner.holdSmall save
          message :: R.Dynamic t (m ()) <- R.holdDyn R.blank
            $ R.ffor response
            $ \case
            Left e -> do
              R.elClass "p" "text-red-500 self-center" $ R.text (Error.message e)
            Right _ -> do
              R.elClass "p" "text-green-500 self-center" $ R.text "Saved"
          status <- R.holdDyn R.blank . R.leftmost . map R.updated $ [spinner, message]
          return ()

