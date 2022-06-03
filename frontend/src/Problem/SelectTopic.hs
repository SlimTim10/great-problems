module Problem.SelectTopic
  ( widget
  , firstTopicId
  ) where

import Common.Lib.Prelude

import qualified Data.Map as Map
import qualified Language.Javascript.JSaddle as JS
import qualified Reflex.Dom.Core as R

import qualified Common.Api.Topic as Topic
import qualified Common.Route as Route
import qualified Widget.Select as Select

firstTopicId :: Integer
firstTopicId = 0

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
widget setTopic = do
  let indent =
        \n txt ->
          cs $
          (concat . replicate (fromIntegral n) $ "- ")
          ++
          cs txt
  let addTopic :: (Integer, Topic.TopicWithLevel) -> Select.DropdownItem Integer -> Select.DropdownItem Integer =
        \(idx, Topic.TopicWithLevel {Topic.twlTopic=t, Topic.twlLevel=lvl}) ->
          Map.insert (Select.DropdownKey idx (Topic.id t)) (indent lvl (Topic.name t))
  Select.widget
    setTopic
    "Topic"
    (Route.Api_Topics :/ (Nothing, mempty))
    (Topic.flattenHierarchy . Topic.topicsToHierarchy)
    addTopic
    firstTopicId
