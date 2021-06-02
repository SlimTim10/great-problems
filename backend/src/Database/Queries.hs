{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Database.Queries
  ( Problem(..)
  , getProblems
  ) where

import qualified Database.PostgreSQL.Simple as SQL
import qualified Data.Aeson as JSON
import GHC.Generics (Generic)

data Problem = Problem
  { id :: Integer
  , title :: String
  , description :: Maybe String
  , contents :: String
  , thumnail_url :: String
  , author_id :: Integer
  , topic_id :: Integer
  } deriving
  ( Eq
  , Generic
  , SQL.FromRow
  , SQL.ToRow
  , JSON.FromJSON
  , JSON.ToJSON
  )

getProblems :: SQL.Connection -> IO ([Problem])
getProblems conn = SQL.query_ conn "SELECT * FROM problems"
