module Database.Queries
  ( getProblems
  ) where

import qualified Database.PostgreSQL.Simple as SQL

import qualified Common.Api as Api

getProblems :: SQL.Connection -> IO ([Api.Problem])
getProblems conn = SQL.query_ conn "SELECT * FROM problems"
