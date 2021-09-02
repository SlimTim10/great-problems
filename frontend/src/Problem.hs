module Problem
  ( widget
  ) where

import qualified Data.Map as Map
import qualified Language.Javascript.JSaddle as JS
import qualified Reflex.Dom.Core as R

import qualified Common.Route as Route
import qualified Common.Api.Topic as Topic
import qualified Util
import Global

widget
  :: ( R.DomBuilder t m
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
  selectTopicWidget

  -- R.elClass "div" "flex-1 h-full flex gap-4" $ do

  --   (options, figures) <- R.elClass "div" "flex-none w-56 flex flex-col gap-4" $ do
  --     options <- R.elClass "div" "border-2 border-gray-300" $ Options.widget
  --     figures <- R.elClass "div" "h-full border-2 border-gray-300" $ Figures.widget
  --     return (options, figures)
      
  --   R.elClass "div" "flex-1 h-full flex flex-col" $ mdo
      
  --     (uploadPrb, convertResponse, loading, errorsToggle) <- R.elClass "div" "bg-gray-100 flex justify-between" $ mdo
  --       uploadPrb <- UploadPrb.widget
  --       DownloadPrb.widget prbName editorContent
  --       prbName <- prbNameWidget
  --       (convertResponse, loading) <- Convert.widget options figures prbName editorContent
  --         <&> R.splitDynPure
  --       errorsToggle <- ErrorsToggle.widget convertResponse loading
  --       return (uploadPrb, convertResponse, loading, errorsToggle)
        
  --     editorContent <- R.elClass "div" "h-full flex" $ mdo
  --       editorContent <- R.elClass "div" "h-full flex-1"$ Editor.widget uploadPrb
  --       let pdfData = maybe "" Convert.pdfContent <$> convertResponse
  --       R.elClass "div" "flex-1" $ PdfViewer.widget pdfData loading convertResponse errorsToggle
  --       return editorContent
        
  --     return ()

selectTopicWidget
  :: forall t m.
     ( R.DomBuilder t m
     , R.HasJSContext (R.Performable m)
     , JS.MonadJSM (R.Performable m)
     , R.PostBuild t m
     , JS.MonadJSM m
     , R.PerformEvent t m
     , R.TriggerEvent t m
     , R.MonadHold t m
     , MonadFix m
     )
  => m ()
selectTopicWidget = do
  R.elClass "p" "font-medium" $ R.text "Topic"
  response :: R.Event t (Maybe [Topic.Topic]) <- Util.getOnload $
    Route.apiHref (Route.Api_Topics :/ mempty)
  let allTopics :: R.Event t [Topic.Topic] = fromMaybe [] <$> response
  let topicsWithChildren :: R.Event t [TopicWithChildren] = topicsToHierarchy <$> allTopics

  R.performEvent_ $ R.ffor topicsWithChildren $ \x -> do
    Util.consoleLog $ ("topicsWithChildren" :: Text)
    Util.consoleLog $ (cs . show $ x :: Text)

  let flattened :: R.Event t [(Integer, Text, Integer)] = flattenHierarchy <$> topicsWithChildren
  R.performEvent_ $ R.ffor flattened $ \x -> do
    Util.consoleLog $ ("flattened" :: Text)
    Util.consoleLog $ (cs . show $ x :: Text)

  let dropdownItems :: R.Event t (Map Integer Text) = hierarchyToDropdownItems <$> flattened
  R.performEvent_ $ R.ffor dropdownItems $ \x -> do
    Util.consoleLog $ ("dropdownItems" :: Text)
    Util.consoleLog $ (cs . show $ x :: Text)

  xs :: R.Dynamic t (Map Integer Text) <- R.holdDyn Map.empty $
    hierarchyToDropdownItems <$> flattenHierarchy <$> topicsToHierarchy <$> allTopics
  void $ R.dropdown 1 xs R.def
  
  return ()
  
  -- allTopics :: R.Dynamic t (Map Integer Text) <- R.holdDyn Map.empty
  --   $ Map.fromList
  --   <$> map (\topic -> (Topic.id topic, Topic.name topic))
  --   <$> fromMaybe []
  --   <$> response
  -- selectedRootTopicId :: R.Dynamic t Integer <- R.value <$> R.dropdown 1 rootTopics R.def
  
  -- let selectedTopicIds :: [R.Dynamic t Integer] = [selectedRootTopicId]
  -- let selectedTopicId = R.leftmost . map R.updated $ selectedTopicIds
  -- let rootTopicsUrl = Route.apiHref (Route.Api_Topics :/ ("parent" =: Just "null"))
  -- url :: R.Dynamic t Text <- R.holdDyn rootTopicsUrl $ (\tid -> Route.apiHref (Route.Api_TopicHierarchy :/ Just tid)) <$> selectedTopicId
  -- let endpoint :: R.Event t Text = R.tagPromptlyDyn url selectedTopicId
  -- response' :: R.Event t (Maybe [[Either Topic.Topic Topic.Topic]]) <- R.getAndDecode endpoint
  -- let topicHierarchy :: R.Event t [[Topic.Topic]] =
  --       filter ((> 0) . length)
  --       <$> map (map (either id id))
  --       <$> fromMaybe []
  --       <$> response'
  -- R.performEvent_ $ R.ffor topicHierarchy $ \t -> do
  --   Util.consoleLog $ ("topicHierarchy" :: Text)
  --   Util.consoleLog $ (cs . show $ t :: Text)

  -- x :: R.Dynamic t [[Topic.Topic]] <- R.holdDyn [] topicHierarchy
  -- void $ R.simpleList x topicsDropdown
  -- return ()
  -- where
  --   topicsDropdown :: R.Dynamic t [Topic.Topic] -> m ()
  --   topicsDropdown topics = Util.dynFor topics $ \ts -> do
  --     let tid = Topic.id . head $ ts
  --     let topics' :: R.Dynamic t (Map Integer Text) = Map.fromList
  --           <$> map (\topic -> (Topic.id topic, Topic.name topic))
  --           <$> topics
  --     void $ R.dropdown tid topics' R.def

data TopicWithChildren = TopicWithChildren
  { topic :: Topic.Topic
  , children :: [TopicWithChildren]
  , level :: Integer
  } deriving (Show)
  
topicsToHierarchy :: [Topic.Topic] -> [TopicWithChildren]
topicsToHierarchy allTopics = map (f 0) rootTopics
  where
    rootTopics = filter (\x -> isNothing (Topic.parent_id x)) allTopics
    f :: Integer -> Topic.Topic -> TopicWithChildren
    f l t = TopicWithChildren t (map (f (l + 1)) $ getChildren t) l
    getChildren :: Topic.Topic -> [Topic.Topic]
    getChildren t = filter (\x -> Topic.parent_id x == Just (Topic.id t)) allTopics

flattenHierarchy :: [TopicWithChildren] -> [(Integer, Text, Integer)]
flattenHierarchy = go []
  where
    go :: [(Integer, Text, Integer)] -> [TopicWithChildren] -> [(Integer, Text, Integer)]
    go acc [] = acc
    go acc topicsWithChildren =
      map (\x -> (Topic.id . topic $ x, Topic.name . topic $ x, level x)) topicsWithChildren
      ++
      (concat . map (go acc . children) $ topicsWithChildren)

hierarchyToDropdownItems :: [(Integer, Text, Integer)] -> Map Integer Text
hierarchyToDropdownItems = foldr f mempty
  where
    f :: (Integer, Text, Integer) -> Map Integer Text -> Map Integer Text
    f (topicId, topicName, topicLevel) = Map.insert topicId (indent topicLevel topicName)
    indent :: Integral a => a -> Text -> Text
    indent n txt = cs $
      (concat . replicate (fromIntegral n) $ "--")
      ++
      cs txt
