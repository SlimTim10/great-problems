module Database.Queries
  ( getProblems
  , getTopics
  , getTopicById
  , getRootTopics
  , getTopicsByParentId
  , getUsers
  , getUserById
  , getTopicHierarchy
  ) where

import qualified Database.PostgreSQL.Simple as SQL
import qualified Database.PostgreSQL.Simple.Types as SQL
import qualified Database.PostgreSQL.Simple.ToField as SQL
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Common.Api.Problem as Problem
import qualified Common.Api.Topic as Topic
import qualified Common.Api.User as User
import qualified Common.Route as Route
import qualified Database.Types.Problem as DbProblem
import qualified Util
import Global

exprFromRouteParam
  :: (SQL.ToField q, Read a)
  => Text -- ^ Param name in route query
  -> SQL.Query -- ^ Target SQL expression with parameter substitution
  -> (a -> q) -- ^ Function to take the previous type to a ToField type (or simply use id with type signature)
  -> Route.Query -- ^ Route query
  -> Maybe (SQL.Query, SQL.Action)
exprFromRouteParam param expr cast routeQuery = do
  y <- Route.paramFromQuery param routeQuery
  Just (expr, SQL.toField $ cast y)

getProblems :: SQL.Connection -> Route.Query -> IO [Problem.Problem]
getProblems conn routeQuery
  | Map.null routeQuery = do
      dbProblems <- SQL.query_ conn "SELECT * FROM problems" :: IO [DbProblem.Problem]
      return $ map
        (\dbProblem ->
           Problem.Problem
           { Problem.id = DbProblem.id dbProblem
           , Problem.summary = DbProblem.summary dbProblem
           , Problem.contents = DbProblem.contents dbProblem
           , Problem.topic = Left $ DbProblem.topic_id dbProblem
           , Problem.author = Left $ DbProblem.author_id dbProblem
           , Problem.topicPath = Nothing
           , Problem.created_at = DbProblem.created_at dbProblem
           , Problem.updated_at = DbProblem.updated_at dbProblem
           })
        dbProblems
  | otherwise = do
      topicExpr <- case Route.paramFromQuery "topic" routeQuery of
        Nothing -> return Nothing
        Just topicId -> do
          topicIds <- getTopicIdPath conn topicId
          return $ Just ("topic_id IN ?", SQL.toField $ SQL.In topicIds)
      let
        -- topicExpr = exprFromRouteParam
        --   "topic"
        --   "topic_id IN ?"
        --   (readMaybe :: String -> Maybe Integer)
        --   (const $ SQL.In topicIds)
        --   routeQuery
        authorExpr = exprFromRouteParam
          "author"
          "author_id = ?"
          (id :: Integer -> Integer)
          routeQuery
        whereClause = mconcat . intersperse " AND " . map fst . catMaybes $ [topicExpr, authorExpr]
        whereParams = map snd . catMaybes $ [topicExpr, authorExpr]
      print =<< SQL.formatQuery conn ("SELECT * FROM problems WHERE " <> whereClause) whereParams
      dbProblems <- if not . all isNothing $ [topicExpr, authorExpr]
        then SQL.query conn ("SELECT * FROM problems WHERE " <> whereClause) whereParams
        else SQL.query_ conn "SELECT * FROM problems"
      return $ map
        (\dbProblem ->
           Problem.Problem
           { Problem.id = DbProblem.id dbProblem
           , Problem.summary = DbProblem.summary dbProblem
           , Problem.contents = DbProblem.contents dbProblem
           , Problem.topic = Left $ DbProblem.topic_id dbProblem
           , Problem.author = Left $ DbProblem.author_id dbProblem
           , Problem.topicPath = Nothing
           , Problem.created_at = DbProblem.created_at dbProblem
           , Problem.updated_at = DbProblem.updated_at dbProblem
           })
        dbProblems
      -- let expands = maybe [] (\x -> Text.splitOn "," x) $ Map.lookup "topic" routeQuery
      -- let
      --   expands = maybe mempty
      --     (maybe mempty (Text.splitOn ","))
      --     $ Map.lookup "topic" routeQuery

      -- x <- if "author" `elem` expands
      --   then sequence $ map expandProblemAuthor problems
      --   else return problems
      -- -- expandedProblems <- maybe problems (\)
      -- return problems
  where
    expandProblemAuthor :: Problem.Problem -> IO (Problem.Problem)
    expandProblemAuthor _ = undefined

getTopics :: SQL.Connection -> IO [Topic.Topic]
getTopics conn = SQL.query_ conn "SELECT * FROM topics"

getTopicById :: SQL.Connection -> Integer -> IO (Maybe Topic.Topic)
getTopicById conn topicId = Util.headMay
  <$> SQL.query conn "SELECT * FROM topics WHERE id = ?" (SQL.Only topicId)

getParentTopic :: SQL.Connection -> Topic.Topic -> IO (Maybe Topic.Topic)
getParentTopic conn topic = case Topic.parent_id topic of
  Nothing -> return Nothing
  Just parentId -> Util.headMay <$> SQL.query conn "SELECT * FROM topics WHERE id = ?" (SQL.Only parentId)

getRootTopics :: SQL.Connection -> IO [Topic.Topic]
getRootTopics conn = SQL.query_ conn "SELECT * FROM topics WHERE parent_id IS NULL"

getTopicsByParentId :: SQL.Connection -> Integer -> IO [Topic.Topic]
getTopicsByParentId conn parentId = SQL.query conn "SELECT * FROM topics WHERE parent_id = ?" (SQL.Only parentId)

-- problemToCard :: SQL.Connection -> Problem.Problem -> IO (ProblemCard.ProblemCard)
-- problemToCard conn problem = do
--   topics <- getTopicById conn (Problem.topic_id problem) >>= \case
--     Nothing -> return []
--     Just topic -> getTopicPath conn topic
--   -- TODO: add proper error handling for finding users
--   user <- getUserById conn (Problem.author_id problem) >>= return . fromMaybe (User.User 0 "" "")
--   return $ ProblemCard.ProblemCard problem topics user

-- getProblemCards :: SQL.Connection -> IO [ProblemCard.ProblemCard]
-- getProblemCards conn = do
--   problems :: [Problem.Problem] <- getProblems conn
--   sequence $ map (problemToCard conn) problems

-- | Get the path pertaining to a given topic, tracing its path to a root topic.
-- For example, given the topic Limits, return [Mathematics, Calculus, Limits]
getTopicPath :: SQL.Connection -> Topic.Topic -> IO [Topic.Topic]
getTopicPath conn topic
  | isNothing (Topic.parent_id topic) = return [topic]
  | otherwise = do
      topicPath <- getParentTopic conn topic >>= \case
        Nothing -> return []
        Just parent -> getTopicPath conn parent
      return $ topicPath ++ [topic]

-- | Same as getTopicPath, but only IDs
getTopicIdPath :: SQL.Connection -> Integer -> IO [Integer]
getTopicIdPath conn topicId = do
  getTopicById conn topicId >>= \case
    Nothing -> return []
    Just topic -> do
      topics <- getTopicPath conn topic
      return $ map Topic.id topics

-- | Get the hierarchy of topics, ending with the children of the given topic. The Either type is used to keep track of irrelevant and relevant topics, respectively Left and Right.
-- For example, given the topic Mathematics, return
--   [ [Left Astronomy, Left Biology, Left Chemistry, Left (Electrical Engineering), Right Mathematics, Left Physics, Left Psychology, Left Statistics]
--   , [Right Calculus, Left (Group Theory)]
--   , [Left (Differential Equations), Left Limits, Left Rates]
--   ]
-- For example, given the topic Calculus, return
--   [ [Left Astronomy, Left Biology, Left Chemistry, Left (Electrical Engineering), Right Mathematics, Left Physics, Left Psychology, Left Statistics]
--   , [Right Calculus, Left (Group Theory)]
--   , [Left (Differential Equations), Left Limits, Left Rates]
--   ]
-- For example, given the topic Limits, return
--   [ [Left Astronomy, Left Biology, Left Chemistry, Left (Electrical Engineering), Right Mathematics, Left Physics, Left Psychology, Left Statistics]
--   , [Right Calculus, Left (Group Theory)]
--   , [Left (Differential Equations), Right Limits, Left Rates]
--   , []
--   ]
getTopicHierarchy :: SQL.Connection -> Topic.Topic -> IO [[Either Topic.Topic Topic.Topic]]
getTopicHierarchy conn topic = do
  topicPath <- getTopicPath conn topic
  siblings <- mapM getSiblings topicPath
  children <- getChildren topic
  return $ siblings ++ [children]
  where
    getSiblings :: Topic.Topic -> IO [Either Topic.Topic Topic.Topic]
    getSiblings t = do
      xs <- case Topic.parent_id t of
        Nothing -> SQL.query_ conn "SELECT * FROM topics WHERE parent_id IS NULL"
        Just pid -> SQL.query conn "SELECT * FROM topics WHERE parent_id = ?" (SQL.Only pid)
      return $ map (\x -> if x == t then Right x else Left x) xs
      
    getChildren :: Topic.Topic -> IO [Either Topic.Topic Topic.Topic]
    getChildren t = do
      xs <- SQL.query conn "SELECT * FROM topics WHERE parent_id = ?" (SQL.Only (Topic.id t))
      return $ map Left xs

getUsers :: SQL.Connection -> IO [User.User]
getUsers conn = SQL.query_ conn "SELECT id, full_name, email FROM users"

getUserById :: SQL.Connection -> Integer -> IO (Maybe User.User)
getUserById conn userId = Util.headMay
  <$> SQL.query conn "SELECT id, full_name, email FROM users WHERE id = ?" (SQL.Only userId)
