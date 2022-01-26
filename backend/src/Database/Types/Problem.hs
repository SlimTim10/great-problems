{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Database.Types.Problem
  ( Problem(..)
  ) where

import Common.Lib.Prelude

import qualified Database.PostgreSQL.Simple as SQL
import qualified Data.Time.Clock as Time
import GHC.Generics (Generic)

data Problem = Problem
  { id :: Integer
  , summary :: Text
  , contents :: Text
  , topic_id :: Integer
  , author_id :: Integer
  , status_id :: Integer
  , created_at :: Time.UTCTime
  , updated_at :: Time.UTCTime
  } deriving
  ( Eq
  , Show
  , Generic
  , SQL.FromRow
  , SQL.ToRow
  )
