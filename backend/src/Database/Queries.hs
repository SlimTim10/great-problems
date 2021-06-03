module Database.Queries
  ( getProblems
  ) where

import qualified Database.PostgreSQL.Simple as SQL

import qualified Common.Api.Problem as Problem

getProblems :: SQL.Connection -> IO ([Problem.Problem])
getProblems conn = SQL.query_ conn "SELECT * FROM problems"
