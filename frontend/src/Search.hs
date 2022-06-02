module Search
  ( widget
  ) where

import qualified Data.Aeson as JSON
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
import qualified Widget.Input as Input
import qualified Problem.SelectTopic as SelectTopic

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
        -- selectTopic $ R.tagPromptlyDyn (R.constDyn $ Search.topicId paramsFromUrl) onload
        let setTopic = R.tagPromptlyDyn (R.constDyn $ Search.topicId paramsFromUrl) onload
        let indent =
              \n txt ->
                cs $
                (concat . replicate (fromIntegral n) $ "- ")
                ++
                cs txt
        let addTopic :: ((Integer, Topic.TopicWithLevel)) -> DropdownItem Integer -> DropdownItem Integer =
              \(idx, Topic.TopicWithLevel {Topic.twlTopic=t, Topic.twlLevel=lvl}) ->
                Map.insert (DropdownKey idx (Topic.id t)) (indent lvl (Topic.name t))
        selectGeneral
          setTopic
          "Topic"
          (Route.Api_Topics :/ (Nothing, mempty))
          (Topic.flattenHierarchy . Topic.topicsToHierarchy)
          addTopic
          (-1)
          
      selectedAuthorId :: R.Dynamic t (Maybe Integer) <- R.elClass "div" "my-4" $ do
        let setAuthor = R.tagPromptlyDyn (R.constDyn $ Search.authorId paramsFromUrl) onload
        let addAuthor :: (Integer, User.User) -> DropdownItem Integer -> DropdownItem Integer =
              \(idx, user) ->
                Map.insert (DropdownKey idx (User.id user)) (CI.original $ User.fullName user)
        selectGeneral
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

-- Based on Problem.SelectTopic
-- TODO: This should be merged with Problem.SelectTopic.widget (and selectAuthor?)
selectTopic
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
  => R.Event t (Maybe Integer) -- ^ Set selected topic by ID
  -> m (R.Dynamic t (Maybe Integer)) -- ^ Topic ID
selectTopic setValue = R.elClass "div" "" $ do
  R.elClass "p" "font-medium mb-2" $ R.text "Topic"
  response :: R.Event t (Maybe [Topic.Topic]) <- Util.getOnload $
    Route.apiHref (Route.Api_Topics :/ (Nothing, mempty))
  let allTopics :: R.Event t [Topic.Topic] = fromMaybe [] <$> response
  dropdownItems :: R.Dynamic t SelectTopic.DropdownItem <- R.holdDyn Map.empty $
    SelectTopic.topicsToDropdownItems
    <$> Topic.flattenHierarchy
    <$> Topic.topicsToHierarchy
    <$> allTopics
  -- Every dropdown item needs an index and topic ID, so we use -1 for the "Any" item (default)
  let anyIdx = -1
  let anyTopicId = -1
  let anyItem = SelectTopic.DropdownKey anyIdx anyTopicId
  let defaultItem = anyItem
  let setValue' :: R.Event t Integer = fromMaybe anyTopicId <$> setValue
  let dropdownItems' = Map.insert anyItem "Any" <$> dropdownItems
  let dropdownKeys :: R.Dynamic t [SelectTopic.DropdownKey] = Map.keys <$> dropdownItems'
  holdSetValue :: R.Dynamic t Integer <- R.holdDyn anyTopicId setValue'
  let setValueKey :: R.Dynamic t SelectTopic.DropdownKey =
        (\v -> fromMaybe defaultItem . find ((== v) . SelectTopic.ddTopicId))
        <$> holdSetValue <*> dropdownKeys
  x <- Input.dropdownClass' "border border-brand-light-gray w-full"
    defaultItem
    dropdownItems'
    (R.updated setValueKey)
  return $ x <&> \ddk -> do
    let tid = SelectTopic.ddTopicId ddk
    if tid == -1
      then Nothing
      else Just tid

-- TODO: clean up

-- data UserDropdownKey = UserDropdownKey
--   { udIdx :: Integer
--   , udUserId :: Integer
--   } deriving (Eq)

-- instance Ord UserDropdownKey where
--   a <= b = udIdx a <= udIdx b

-- type UserDropdownItem = Map UserDropdownKey Text

-- selectAuthor
--   :: forall t m.
--      ( R.DomBuilder t m
--      , R.HasJSContext (R.Performable m)
--      , JS.MonadJSM (R.Performable m)
--      , R.PostBuild t m
--      , JS.MonadJSM m
--      , R.PerformEvent t m
--      , R.TriggerEvent t m
--      , R.MonadHold t m
--      , MonadFix m
--      )
--   => R.Event t (Maybe Integer) -- ^ Set selected author by ID
--   -> m (R.Dynamic t (Maybe Integer)) -- ^ Author ID
-- selectAuthor setValue = R.elClass "div" "" $ do
--   R.elClass "p" "font-medium mb-2" $ R.text "Author"
--   response :: R.Event t (Maybe [User.User]) <- Util.getOnload $
--     Route.apiHref (Route.Api_Users :/ Nothing)
--   let allUsers :: R.Event t [User.User] = fromMaybe [] <$> response
--   dropdownItems :: R.Dynamic t UserDropdownItem <- R.holdDyn Map.empty $
--     usersToDropdownItems
--     <$> allUsers
--   -- Every dropdown item needs an index and user ID, so we use -1 for the "Any" item (default)
--   let anyIdx = -1
--   let anyUserId = -1
--   let anyItem = UserDropdownKey anyIdx anyUserId
--   let defaultItem = anyItem
--   let setValue' :: R.Event t Integer = fromMaybe anyUserId <$> setValue
--   let dropdownItems' = Map.insert anyItem "Any" <$> dropdownItems
--   let dropdownKeys :: R.Dynamic t [UserDropdownKey] = Map.keys <$> dropdownItems'
--   holdSetValue :: R.Dynamic t Integer <- R.holdDyn anyUserId setValue'
--   let setValueKey :: R.Dynamic t UserDropdownKey =
--         (\v -> fromMaybe defaultItem . find ((== v) . udUserId))
--         <$> holdSetValue <*> dropdownKeys
--   x <- Input.dropdownClass' "border border-brand-light-gray w-full"
--     defaultItem
--     dropdownItems'
--     (R.updated setValueKey)
--   return $ x <&> \ddk -> do
--     let uid = udUserId ddk
--     if uid == -1
--       then Nothing
--       else Just uid
      
--   where
--     usersToDropdownItems :: [User.User] -> UserDropdownItem
--     usersToDropdownItems = foldr addItem mempty . zip [1 ..]
--       where
--         addItem :: (Integer, User.User) -> UserDropdownItem -> UserDropdownItem
--         addItem (idx, user) = Map.insert (UserDropdownKey idx (User.id user)) (CI.original $ User.fullName user)

-- TODO: rename
selectGeneral
  :: forall t m a b c.
     ( R.DomBuilder t m
     , R.HasJSContext (R.Performable m)
     , JS.MonadJSM (R.Performable m)
     , R.PostBuild t m
     , JS.MonadJSM m
     , R.PerformEvent t m
     , R.TriggerEvent t m
     , R.MonadHold t m
     , MonadFix m
     , Eq a
     , JSON.FromJSON b
     )
  => R.Event t (Maybe a) -- ^ Set selected item by value
  -> Text -- ^ Dropdown title
  -> Ob.R Route.Api -- ^ Route to fetch values to populate
  -> ([b] -> [c]) -- ^ Transform the fetched values before adding to dropdown (use id if not needed)
  -> ((Integer, c) -> DropdownItem a -> DropdownItem a) -- ^ How to add an item to the dropdown
  -> a -- ^ Value for "Any" item
  -> m (R.Dynamic t (Maybe a)) -- ^ Selected dropdown value
selectGeneral setValue title route transformValues addItem anyValue = R.elClass "div" "" $ do
  R.elClass "p" "font-medium mb-2" $ R.text title
  response :: R.Event t (Maybe [b]) <- Util.getOnload $
    Route.apiHref route
  let allValues :: R.Event t [b] = fromMaybe [] <$> response
  dropdownItems :: R.Dynamic t (DropdownItem a) <- R.holdDyn Map.empty $
    valuesToDropdownItems
    . transformValues
    <$> allValues
  -- Every dropdown item needs an index and user ID, so we use -1 for the "Any" item (default)
  let anyIdx = -1
  let anyItem = DropdownKey anyIdx anyValue
  let defaultItem = anyItem
  let setValue' :: R.Event t a = fromMaybe anyValue <$> setValue
  let dropdownItems' = Map.insert anyItem "Any" <$> dropdownItems
  let dropdownKeys :: R.Dynamic t [DropdownKey a] = Map.keys <$> dropdownItems'
  holdSetValue :: R.Dynamic t a <- R.holdDyn anyValue setValue'
  let setValueKey :: R.Dynamic t (DropdownKey a) =
        (\v -> fromMaybe defaultItem . find ((== v) . dkValue))
        <$> holdSetValue <*> dropdownKeys
  x <- Input.dropdownClass' "border border-brand-light-gray w-full"
    defaultItem
    dropdownItems'
    (R.updated setValueKey)
  return $ x <&> \ddk -> do
    let v = dkValue ddk
    if v == anyValue
      then Nothing
      else Just v
      
  where
    valuesToDropdownItems :: [c] -> DropdownItem a
    valuesToDropdownItems = foldr addItem mempty . zip [1 ..]

type DropdownItem a = Map (DropdownKey a) Text

data DropdownKey a = DropdownKey
  { dkIdx :: Integer
  , dkValue :: a
  } deriving (Eq)

instance Eq a => Ord (DropdownKey a) where
  a <= b = dkIdx a <= dkIdx b
