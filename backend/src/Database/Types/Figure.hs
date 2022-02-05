{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
module Database.Types.Figure
  ( Figure(..)
  ) where

import Common.Lib.Prelude

import qualified Data.ByteString as B
import qualified Database.PostgreSQL.Simple as SQL
import qualified Data.Time.Clock as Time
import GHC.Generics (Generic)

data Figure = Figure
  { id :: Integer
  , name :: Text
  , contents :: SQL.Binary B.ByteString
  , problem_id :: Integer
  , created_at :: Time.UTCTime
  , updated_at :: Time.UTCTime
  } deriving
  ( Eq
  , Show
  , Generic
  , SQL.FromRow
  , SQL.ToRow
  )
