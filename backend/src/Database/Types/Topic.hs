{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Database.Types.Topic
  ( Topic(..)
  ) where

import qualified Database.PostgreSQL.Simple as SQL
import qualified Data.Aeson as JSON
import qualified Data.CaseInsensitive as CI
import qualified Data.Time.Clock as Time
import GHC.Generics (Generic)

import Global

data Topic = Topic
  { id :: Integer
  , name :: Text
  , parent_id :: Maybe Integer
  } deriving
  ( Eq
  , Show
  , Generic
  , SQL.FromRow
  , SQL.ToRow
  , JSON.FromJSON
  , JSON.ToJSON
  )
