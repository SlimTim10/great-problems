{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Database.Types.Problem
  ( Problem(..)
  ) where

import qualified Database.PostgreSQL.Simple as SQL
import qualified Data.Time.Clock as Time
import GHC.Generics (Generic)

import Global

data Problem = Problem
  { id :: Integer
  , summary :: Text
  , content :: Text
  , topic_id :: Integer
  , author_id :: Integer
  , created_at :: Time.UTCTime
  , updated_at :: Time.UTCTime
  } deriving
  ( Eq
  , Show
  , Generic
  , SQL.FromRow
  , SQL.ToRow
  )
