module Problem.SelectTopic
  ( widget
  ) where

import Common.Lib.Prelude
import qualified Frontend.Lib.Util as Util

import qualified Data.Map as Map
import qualified Language.Javascript.JSaddle as JS
import qualified Reflex.Dom.Core as R

import qualified Common.Api.Topic as Topic
import qualified Common.Route as Route
import qualified Widget.Input as Input

widget
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
  => R.Event t Integer
  -> m (R.Dynamic t Integer)
widget setValue = R.elClass "div" "" $ do
  R.elClass "p" "font-medium mb-2" $ R.text "Topic"
  response :: R.Event t (Maybe [Topic.Topic]) <- Util.getOnload $
    Route.apiHref (Route.Api_Topics :/ mempty)
  let allTopics :: R.Event t [Topic.Topic] = fromMaybe [] <$> response
  dropdownItems :: R.Dynamic t (Map Integer Text) <- R.holdDyn Map.empty $
    hierarchyToDropdownItems <$> flattenHierarchy <$> topicsToHierarchy <$> allTopics
  Input.dropdownClass' "border border-brand-light-gray w-full" 1 dropdownItems setValue
  
data TopicWithChildren = TopicWithChildren
  { topic :: Topic.Topic
  , children :: [TopicWithChildren]
  , level :: Integer
  } deriving (Show)
  
topicsToHierarchy :: [Topic.Topic] -> [TopicWithChildren]
topicsToHierarchy allTopics = map (f 0) rootTopics
  where
    rootTopics = filter (\x -> isNothing (Topic.parentId x)) allTopics
    f :: Integer -> Topic.Topic -> TopicWithChildren
    f l t = TopicWithChildren t (map (f (l + 1)) $ getChildren t) l
    getChildren :: Topic.Topic -> [Topic.Topic]
    getChildren t = filter (\x -> Topic.parentId x == Just (Topic.id t)) allTopics

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

