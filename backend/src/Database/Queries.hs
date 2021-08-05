module Database.Queries
  ( getProblems
  , getTopics
  , getRootTopics
  , getProblemTiles
  ) where

import qualified Database.PostgreSQL.Simple as SQL

import qualified Common.Api.Problem as Problem
import qualified Common.Api.Topic as Topic
import qualified Common.Api.ProblemTile as ProblemTile
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

problemToTile :: SQL.Connection -> Problem.Problem -> IO (ProblemTile.ProblemTile)
problemToTile conn problem = do
  topics <- getTopicById conn (Problem.topic_id problem) >>= \case
    Nothing -> return []
    Just topic -> getTopicBranch conn topic
  return $ ProblemTile.ProblemTile problem topics

getProblemTiles :: SQL.Connection -> IO ([ProblemTile.ProblemTile])
getProblemTiles conn = do
  problems :: [Problem.Problem] <- getProblems conn
  sequence $ map (problemToTile conn) problems

getTopicBranch :: SQL.Connection -> Topic.Topic -> IO ([Topic.Topic])
getTopicBranch conn topic
  | isNothing (Topic.parent_id topic) = return [topic]
  | otherwise = do
      topicBranch <- getParentTopic conn topic >>= \case
        Nothing -> return []
        Just parentTopic -> getTopicBranch conn parentTopic
      return (topicBranch ++ [topic])
