module AdminDashboard.Topics
  ( widget
  ) where

import qualified Language.Javascript.JSaddle as JS
import qualified Reflex.Dom.Core as R

import Common.Lib.Prelude
import qualified AdminDashboard.Util as DashUtil
import qualified Frontend.Lib.Util as Util
import qualified Common.Route as Route
import qualified Common.Api.Error as Error
import qualified Common.Api.Topic as Topic
import qualified Frontend.Lib.Api as Api
import qualified Widget.Spinner as Spinner
import qualified Widget.Button as Button
import qualified Widget.Input as Input

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
  DashUtil.title "Topics"
  response :: R.Event t (Maybe [Topic.Topic]) <- Util.getOnload
    $ Route.apiHref $ Route.Api_Topics :/ (Nothing, mempty)
  let resTopics :: R.Event t [Topic.Topic] = fromMaybe [] <$> response
  topics :: R.Dynamic t [Topic.TopicWithLevel] <- R.holdDyn [] $
    Topic.flattenHierarchy <$> Topic.topicsToHierarchy <$> resTopics
  R.elClass "div" "flex justify-center" $ do
    R.elClass "table" "border-collapse table-auto" $ do
      R.el "thead" $ do
        R.el "tr" $ do
          R.elClass "th" "text-left pr-4" $ R.text "ID"
          R.elClass "th" "text-left pr-4" $ R.text "Name"
          R.elClass "th" "text-left" $ R.text "Parent ID"
          R.elClass "th" "text-left" $ R.text "Action"
      R.elClass "tbody" "" $ do
        void $ R.simpleList topics viewTopic
        viewCreateTopic
  where
    viewTopic :: R.Dynamic t Topic.TopicWithLevel -> m ()
    viewTopic topic = do
      R.elClass "tr" "" $ do
        Util.dynFor topic $ \x -> do
          R.elClass "td" "" $ do
            R.text $ cs . show . Topic.id . Topic.twlTopic $ x
          name :: R.Dynamic t Text <- R.elClass "td" "" $ do
            R.text $ cs . concat . replicate (fromIntegral $ Topic.twlLevel x) $ "- "
            fmap R.value $ R.inputElement $
              R.def
              & R.inputElementConfig_elementConfig . R.elementConfig_initialAttributes .~
              ( "type" =: "text"
                <> "class" =: "border px-1 mr-4"
              )
              & R.inputElementConfig_initialValue .~ (Topic.name . Topic.twlTopic $ x)
          parentIdTxt :: R.Dynamic t Text <- R.elClass "td" "" $ do
            let parentIdTxt' = case Topic.parentId . Topic.twlTopic $ x of
                  Nothing -> ""
                  Just parentId -> cs . show $ parentId
            fmap R.value $ R.inputElement $
              R.def
              & R.inputElementConfig_elementConfig . R.elementConfig_initialAttributes .~
              ( "type" =: "text"
                <> "class" =: "border px-1 w-16 mr-4"
              )
              & R.inputElementConfig_initialValue .~ parentIdTxt'
          (save, delete) <- R.elClass "td" "" $ do
            save :: R.Event t () <- Button.primarySmallClass' "Save" "mr-1"
            delete :: R.Event t () <- Button.secondarySmall' "Delete"
            return (save, delete)

          saveResponse :: R.Event t (Either Error.Error ()) <- do
            let topicId :: Integer = Topic.id . Topic.twlTopic $ x
            let parentId :: R.Dynamic t (Maybe Integer) = parentIdTxt <&> \case
                  "" -> Nothing
                  txt -> Just . read . cs $ txt
            let topic' :: R.Dynamic t Topic.Topic = Topic.Topic
                  <$> R.constDyn topicId
                  <*> name
                  <*> parentId
            Api.postRequest
              topic'
              save
              (Route.Api_Topics :/ (Just topicId, mempty))
              id
          saveMessage :: R.Dynamic t (m ()) <- R.holdDyn R.blank
            $ R.ffor saveResponse
            $ \case
            Left e -> do
              R.elClass "p" "text-red-500 self-center" $ R.text (Error.message e)
            Right _ -> do
              R.elClass "p" "text-green-500 self-center" $ R.text "Saved"

          deleteResponse :: R.Event t (Either Error.Error ()) <- do
            let topicId :: Integer = Topic.id . Topic.twlTopic $ x
            Api.deleteRequest
              (const (Route.Api_Topics :/ (Just topicId, mempty)) <$> delete)
          deleteMessage :: R.Dynamic t (m ()) <- R.holdDyn R.blank
            $ R.ffor deleteResponse
            $ \case
            Left e -> do
              R.elClass "p" "text-red-500 self-center" $ R.text (Error.message e)
            Right _ -> do
              R.elClass "p" "text-green-500 self-center" $ R.text "Deleted"
            
          spinner <- Spinner.holdSmall $ R.leftmost [save, delete]
          let placeholder = R.elClass "div" "w-16" $ R.blank
          status <- R.holdDyn placeholder . R.leftmost . map R.updated $ [spinner, saveMessage, deleteMessage]
          R.dyn_ status

    viewCreateTopic = do
      R.el "tr" $ do
        R.elClass "td" "" $ R.blank
        name :: R.Dynamic t Text <- R.elClass "td" "" $ do
          Input.textClass "border px-1 mr-4"
        parentIdTxt :: R.Dynamic t Text <- R.elClass "td" "" $ do
          Input.textClass "border px-1 w-16 mr-4"
        create :: R.Event t () <- R.elClass "td" "" $ do
          Button.primarySmall' "Create"

        createResponse :: R.Event t (Either Error.Error ()) <- do
          let parentId :: R.Dynamic t (Maybe Integer) = parentIdTxt <&> \case
                "" -> Nothing
                txt -> Just . read . cs $ txt
          let newTopic :: R.Dynamic t Topic.NewTopic = Topic.NewTopic
                <$> name
                <*> parentId
          Api.postRequest
            newTopic
            create
            (Route.Api_Topics :/ (Nothing, mempty))
            id

        spinner <- Spinner.holdSmall create
        message :: R.Dynamic t (m ()) <- R.holdDyn R.blank
          $ R.ffor createResponse
          $ \case
          Left e -> do
            R.elClass "p" "text-red-500 self-center" $ R.text (Error.message e)
          Right _ -> do
            R.elClass "p" "text-green-500 self-center" $ R.text "Created"
        let placeholder = R.elClass "div" "w-16" $ R.blank
        status <- R.holdDyn placeholder . R.leftmost . map R.updated $ [spinner, message]
        R.dyn_ status
        
