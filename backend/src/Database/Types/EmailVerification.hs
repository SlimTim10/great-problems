{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Database.Types.EmailVerification
  ( EmailVerification(..)
  ) where

import Backend.Lib.Prelude

import qualified Database.PostgreSQL.Simple as SQL
import qualified Data.Time.Clock as Time
import GHC.Generics (Generic)

data EmailVerification = EmailVerification
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
