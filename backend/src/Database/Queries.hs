module Database.Queries
  ( getProblems
  , getTopics
  , getTopicById
  , getRootTopics
  , getTopicsByParentId
  , getProblemCards
  , getUsers
  , getUserById
  , getTopicHierarchy
  ) where

import qualified Database.PostgreSQL.Simple as SQL

import qualified Common.Api.Problem as Problem
import qualified Common.Api.Topic as Topic
import qualified Common.Api.ProblemCard as ProblemCard
import qualified Common.Api.User as User
import qualified Util
import Global

getProblems :: SQL.Connection -> IO ([Problem.Problem])
getProblems conn = SQL.query_ conn "SELECT * FROM problems"

getTopics :: SQL.Connection -> IO ([Topic.Topic])
getTopics conn = SQL.query_ conn "SELECT * FROM topics"

getTopicById :: SQL.Connection -> Integer -> IO (Maybe Topic.Topic)
getTopicById conn topicId = Util.headMay
  <$> SQL.query conn "SELECT * FROM topics WHERE id = ?" (SQL.Only topicId)

getParentTopic :: SQL.Connection -> Topic.Topic -> IO (Maybe Topic.Topic)
getParentTopic conn topic = case Topic.parent_id topic of
  Nothing -> return Nothing
  Just parentId -> Util.headMay <$> SQL.query conn "SELECT * FROM topics WHERE id = ?" (SQL.Only parentId)

getRootTopics :: SQL.Connection -> IO ([Topic.Topic])
getRootTopics conn = SQL.query_ conn "SELECT * FROM topics WHERE parent_id IS NULL"

getTopicsByParentId :: SQL.Connection -> Integer -> IO ([Topic.Topic])
getTopicsByParentId conn parentId = SQL.query conn "SELECT * FROM topics WHERE parent_id = ?" (SQL.Only parentId)

problemToCard :: SQL.Connection -> Problem.Problem -> IO (ProblemCard.ProblemCard)
problemToCard conn problem = do
  topics <- getTopicById conn (Problem.topic_id problem) >>= \case
    Nothing -> return []
    Just topic -> getTopicBranch conn topic
  -- TODO: add proper error handling for finding users
  user <- getUserById conn (Problem.author_id problem) >>= return . fromMaybe (User.User 0 "" "")
  return $ ProblemCard.ProblemCard problem topics user

getProblemCards :: SQL.Connection -> IO ([ProblemCard.ProblemCard])
getProblemCards conn = do
  problems :: [Problem.Problem] <- getProblems conn
  sequence $ map (problemToCard conn) problems

-- | Get the branch pertaining to a given topic, tracing its path to a root topic.
-- For example, given the topic Limits, return [Mathematics, Calculus, Limits]
getTopicBranch :: SQL.Connection -> Topic.Topic -> IO ([Topic.Topic])
getTopicBranch conn topic
  | isNothing (Topic.parent_id topic) = return [topic]
  | otherwise = do
      topicBranch <- getParentTopic conn topic >>= \case
        Nothing -> return []
        Just parent -> getTopicBranch conn parent
      return $ topicBranch ++ [topic]

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
getTopicHierarchy :: SQL.Connection -> Topic.Topic -> IO ([[Either Topic.Topic Topic.Topic]])
getTopicHierarchy conn topic = do
  topicBranch <- getTopicBranch conn topic
  siblings <- mapM getSiblings topicBranch
  children <- getChildren topic
  return $ siblings ++ [children]
  where
    getSiblings :: Topic.Topic -> IO ([Either Topic.Topic Topic.Topic])
    getSiblings t = do
      xs <- case Topic.parent_id t of
        Nothing -> SQL.query_ conn "SELECT * FROM topics WHERE parent_id IS NULL"
        Just pid -> SQL.query conn "SELECT * FROM topics WHERE parent_id = ?" (SQL.Only pid)
      return $ map (\x -> if x == t then Right x else Left x) xs
      
    getChildren :: Topic.Topic -> IO ([Either Topic.Topic Topic.Topic])
    getChildren t = do
      xs <- SQL.query conn "SELECT * FROM topics WHERE parent_id = ?" (SQL.Only (Topic.id t))
      return $ map Left xs

getUsers :: SQL.Connection -> IO ([User.User])
getUsers conn = SQL.query_ conn "SELECT id, full_name, email FROM users"

getUserById :: SQL.Connection -> Integer -> IO (Maybe User.User)
getUserById conn userId = Util.headMay
  <$> SQL.query conn "SELECT id, full_name, email FROM users WHERE id = ?" (SQL.Only userId)
