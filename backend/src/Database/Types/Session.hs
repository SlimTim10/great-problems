{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Database.Types.Session
  ( Session(..)
  ) where

import qualified Database.PostgreSQL.Simple as SQL
import qualified Data.Time.Clock as Time
import GHC.Generics (Generic)

import Global

data Session = Session
  { id :: Text
  , user_id :: Integer
  , created_at :: Time.UTCTime
  } deriving
  ( Eq
  , Show
  , Generic
  , SQL.FromRow
  , SQL.ToRow
  )