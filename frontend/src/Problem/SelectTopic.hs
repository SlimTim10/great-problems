module Problem.SelectTopic
  ( widget
  , firstTopicId
  ) where

import Common.Lib.Prelude
import qualified Frontend.Lib.Util as Util

import qualified Data.Map as Map
import qualified Language.Javascript.JSaddle as JS
import qualified Reflex.Dom.Core as R

import qualified Common.Api.Topic as Topic
import qualified Common.Route as Route
import qualified Widget.Input as Input

firstTopicId :: Integer
firstTopicId = 0

data DropdownKey = DropdownKey
  { ddIdx :: Integer
  , ddTopicId :: Integer
  } deriving (Eq)

instance Ord DropdownKey where
  a <= b = ddIdx a <= ddIdx b

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
  => R.Event t Integer -- ^ Set selected topic by ID
  -> m (R.Dynamic t Integer) -- ^ Topic ID
widget setValue = R.elClass "div" "" $ do
  R.elClass "p" "font-medium mb-2" $ R.text "Topic"
  response :: R.Event t (Maybe [Topic.Topic]) <- Util.getOnload $
    Route.apiHref (Route.Api_Topics :/ mempty)
  let allTopics :: R.Event t [Topic.Topic] = fromMaybe [] <$> response
  dropdownItems :: R.Dynamic t (Map DropdownKey Text) <- R.holdDyn Map.empty $
    hierarchyToDropdownItems <$> flattenHierarchy <$> topicsToHierarchy <$> allTopics
  let dropdownKeys :: R.Dynamic t [DropdownKey] = Map.keys <$> dropdownItems
  holdSetValue :: R.Dynamic t Integer <- R.holdDyn firstTopicId setValue
  let setValueKey :: R.Dynamic t DropdownKey =
        (\v -> fromMaybe (DropdownKey 1 firstTopicId) . find ((== v) . ddTopicId))
        <$> holdSetValue <*> dropdownKeys
  x <- Input.dropdownClass' "border border-brand-light-gray w-full"
    (DropdownKey 1 firstTopicId)
    dropdownItems
    (R.updated setValueKey)
  return $ ddTopicId <$> x
  
data TopicWithChildren = TopicWithChildren
  { tcTopic :: Topic.Topic
  , tcChildren :: [TopicWithChildren]
  , tcLevel :: Integer
  } deriving (Show)

data TopicWithLevel = TopicWithLevel
  { tlTopic :: Topic.Topic
  , tlLevel :: Integer
  }

data DropdownItem = DropdownItem
  { ddiIdx :: Integer
  , ddiTopic :: TopicWithLevel
  }

topicsToHierarchy :: [Topic.Topic] -> [TopicWithChildren]
topicsToHierarchy allTopics = map (f 0) rootTopics
  where
    rootTopics = filter (\x -> isNothing (Topic.parentId x)) allTopics
    f :: Integer -> Topic.Topic -> TopicWithChildren
    f lvl t = TopicWithChildren t (map (f (lvl + 1)) $ getChildren t) lvl
    getChildren :: Topic.Topic -> [Topic.Topic]
    getChildren t = filter (\x -> Topic.parentId x == Just (Topic.id t)) allTopics

flattenHierarchy
  :: [TopicWithChildren]
  -> [DropdownItem]
flattenHierarchy = zipWith DropdownItem [1 ..] . concatMap f
  where
    f :: TopicWithChildren -> [TopicWithLevel]
    f x =
      [(\x' -> TopicWithLevel (tcTopic x') (tcLevel x')) x]
      ++
      (concatMap f . tcChildren) x

hierarchyToDropdownItems :: [DropdownItem] -> Map DropdownKey Text
hierarchyToDropdownItems = foldr f mempty
  where
    f :: DropdownItem -> Map DropdownKey Text -> Map DropdownKey Text
    f (DropdownItem {ddiIdx=idx, ddiTopic=TopicWithLevel {tlTopic=t, tlLevel=lvl} }) =
      Map.insert (DropdownKey idx (Topic.id t)) (indent lvl (Topic.name t))
    indent :: Integral a => a -> Text -> Text
    indent n txt = cs $
      (concat . replicate (fromIntegral n) $ "- ")
      ++
      cs txt

