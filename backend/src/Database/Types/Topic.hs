{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Database.Types.Topic
  ( Topic(..)
  ) where

import qualified Database.PostgreSQL.Simple as SQL
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
  )
