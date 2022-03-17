module AdminDashboard.MetaSettings
  ( widget
  ) where

import qualified Language.Javascript.JSaddle as JS
import qualified Reflex.Dom.Core as R

import Common.Lib.Prelude
import qualified AdminDashboard.Util as DashUtil
import qualified Common.Api.MetaSetting as MetaSetting
import qualified Frontend.Lib.Util as Util
import qualified Common.Route as Route
import qualified Common.Api.Error as Error
import qualified Frontend.Lib.Api as Api
import qualified Widget.Spinner as Spinner
import qualified Widget.Button as Button

widget
  :: forall t m.
     ( R.DomBuilder t m
     , R.PostBuild t m
     , R.MonadHold t m
     , MonadFix m
     , JS.MonadJSM m
     , JS.MonadJSM (R.Performable m)
     , R.HasJSContext (R.Performable m)
     , R.PerformEvent t m
     , R.TriggerEvent t m
     )
  => m ()
widget = do
  DashUtil.title "Meta Settings"
  response :: R.Event t (Maybe [MetaSetting.MetaSetting]) <- Util.getOnload
    $ Route.apiHref $ Route.Api_MetaSettings :/ Nothing
  metaSettings :: R.Dynamic t [MetaSetting.MetaSetting] <- R.holdDyn [] $ fromMaybe [] <$> response
  R.elClass "ul" "flex flex-col gap-4" $ do
    void $ R.simpleList metaSettings viewMetaSetting
  where
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
