module Search
  ( widget
  ) where

import qualified Web.KeyCode as Key
import qualified Data.CaseInsensitive as CI
import qualified Data.Map as Map
import qualified Control.Monad.Fix as Fix
import qualified Language.Javascript.JSaddle as JS
import qualified Obelisk.Route.Frontend as Ob
import qualified Reflex.Dom.Core as R

import Common.Lib.Prelude
import qualified Frontend.Lib.Util as Util
import qualified Common.Route as Route
import qualified Common.Api.Search as Search
import qualified Common.Api.Topic as Topic
import qualified Common.Api.User as User
import qualified Tabs
import qualified ProblemCards
import qualified Common.Api.Problem as Problem
import qualified Widget.Button as Button
import qualified Widget.Select as Select

widget
  :: forall t m js.
     ( R.DomBuilder t m
     , R.PerformEvent t m
     , JS.MonadJSM (R.Performable m)
     , R.HasJSContext (R.Performable m)
     , R.TriggerEvent t m
     , R.MonadHold t m
     , R.PostBuild t m
     , Ob.SetRoute t (Ob.R Route.FrontendRoute) m
     , Ob.RouteToUrl (Ob.R Route.FrontendRoute) m
     , R.Prerender js t m
     , JS.MonadJSM m
     , Fix.MonadFix m
     )
  => Search.Params
  -> m ()
widget paramsFromUrl = do
  R.elClass "div" "bg-brand-light-gray flex justify-center py-4" $ do
    R.elClass "div" "max-w-screen-lg flex flex-col items-center" $ do
      R.el "p" $ R.text "Search bar"
      searchTermInput <- R.inputElement
        $ R.def & R.inputElementConfig_elementConfig . R.elementConfig_initialAttributes .~
        ( "type" =: "search"
          <> "placeholder" =: "Search..."
          <> "class" =: "border rounded h-8 w-96 px-1"
        ) & R.inputElementConfig_initialValue .~ fromMaybe "" (Search.query paramsFromUrl)
      let searchTerm :: R.Dynamic t (Maybe Text) = fmap textToMaybe . R.value $ searchTermInput
      onload :: R.Event t () <- R.getPostBuild
      selectedTopicId :: R.Dynamic t (Maybe Integer) <- R.elClass "div" "my-4" $ do
        let setTopic = R.tagPromptlyDyn (R.constDyn $ Search.topicId paramsFromUrl) onload
        let indent =
              \n txt ->
                cs $
                (concat . replicate (fromIntegral n) $ "- ")
                ++
                cs txt
        let addTopic :: ((Integer, Topic.TopicWithLevel)) -> Select.DropdownItem Integer -> Select.DropdownItem Integer =
              \(idx, Topic.TopicWithLevel {Topic.twlTopic=t, Topic.twlLevel=lvl}) ->
                Map.insert (Select.DropdownKey idx (Topic.id t)) (indent lvl (Topic.name t))
        Select.widgetWithAny
          setTopic
          "Topic"
          (Route.Api_Topics :/ (Nothing, mempty))
          (Topic.flattenHierarchy . Topic.topicsToHierarchy)
          addTopic
          (-1)
          
      selectedAuthorId :: R.Dynamic t (Maybe Integer) <- R.elClass "div" "my-4" $ do
        let setAuthor = R.tagPromptlyDyn (R.constDyn $ Search.authorId paramsFromUrl) onload
        let addAuthor :: (Integer, User.User) -> Select.DropdownItem Integer -> Select.DropdownItem Integer =
              \(idx, user) ->
                Map.insert (Select.DropdownKey idx (User.id user)) (CI.original $ User.fullName user)
        Select.widgetWithAny
          setAuthor
          "Author"
          (Route.Api_Users :/ Nothing)
          id
          addAuthor
          (-1)
      search :: R.Event t () <- Button.primary' "Search"
      let params :: R.Dynamic t Search.Params = Search.Params
            <$> searchTerm
            <*> selectedTopicId
            <*> selectedAuthorId
            <*> R.constDyn (Search.collection paramsFromUrl)
      Util.dynFor params $ \params' ->
        Ob.setRoute $ (Route.FrontendRoute_Search :/ Search.paramsToQuery params')
        <$ (R.leftmost [search, R.keydown Key.Enter searchTermInput])
  let defaultTab = Search.Problems
  case fromMaybe defaultTab (Search.collection paramsFromUrl) of
    Search.Problems -> do
      Tabs.widget Tabs.Problems paramsFromUrl
      problems :: R.Dynamic t [Problem.Problem] <- getProblems paramsFromUrl
      ProblemCards.widget problems
    Search.ProblemSets -> do
      -- Problem sets not yet implemented
      Tabs.widget Tabs.ProblemSets paramsFromUrl
    Search.Courses -> do
    -- Courses not yet implemented
      -- Tabs.widget Tabs.Courses paramsFromUrl
      return ()
  
getProblems
  :: ( R.PostBuild t m
     , JS.MonadJSM (R.Performable m)
     , R.PerformEvent t m
     , R.HasJSContext (R.Performable m)
     , R.TriggerEvent t m
     , R.MonadHold t m
     , JS.MonadJSM m
     )
  => Search.Params
  -> m (R.Dynamic t [Problem.Problem])
getProblems params = do
  response :: R.Event t (Maybe [Problem.Problem]) <- Util.getOnload
    $ Route.apiHref
    $ Route.Api_Problems :/ (Nothing, Search.paramsToQuery params)
  R.holdDyn [] $ fromMaybe [] <$> response
