{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Database.Types.ResetPassword
  ( ResetPassword(..)
  ) where

import Common.Lib.Prelude

import qualified Database.PostgreSQL.Simple as SQL
import qualified Data.Time.Clock as Time
import GHC.Generics (Generic)

data ResetPassword = ResetPassword
  { id :: Integer
  , secret :: Text
  , user_id :: Integer
  , created_at :: Time.UTCTime
  } deriving
  ( Eq
  , Show
  , Generic
  , SQL.FromRow
  , SQL.ToRow
  )
