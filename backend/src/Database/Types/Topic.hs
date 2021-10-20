{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Database.Types.Topic
  ( Topic(..)
  ) where

import Backend.Lib.Prelude

import qualified Database.PostgreSQL.Simple as SQL
import GHC.Generics (Generic)

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
