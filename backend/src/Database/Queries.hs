module Database.Queries
  ( getProblems
  , getTopics
  , getRootTopics
  ) where

import qualified Database.PostgreSQL.Simple as SQL

import qualified Common.Api.Problem as Problem
import qualified Common.Api.Topic as Topic

getProblems :: SQL.Connection -> IO ([Problem.Problem])
getProblems conn = SQL.query_ conn "SELECT * FROM problems"

getTopics :: SQL.Connection -> IO ([Topic.Topic])
getTopics conn = SQL.query_ conn "SELECT * FROM topics"

getRootTopics :: SQL.Connection -> IO ([Topic.Topic])
getRootTopics conn = SQL.query_ conn "SELECT * FROM topics WHERE parent_id IS NULL"
