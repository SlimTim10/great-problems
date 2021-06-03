{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Common.Api.Problem
  ( Problem(..)
  ) where

import qualified Database.PostgreSQL.Simple as SQL
import qualified Data.Aeson as JSON
import GHC.Generics (Generic)

import Global

data Problem = Problem
  { id :: Integer
  , title :: Text
  , description :: Maybe Text
  , contents :: Text
  , thumnail_url :: Text
  , author_id :: Integer
  , topic_id :: Integer
  } deriving
  ( Eq
  , Show
  , Generic
  , SQL.FromRow
  , SQL.ToRow
  , JSON.FromJSON
  , JSON.ToJSON
  )
