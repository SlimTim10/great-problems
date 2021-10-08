{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
module Database.Types.Figure
  ( Figure(..)
  ) where

import qualified Data.ByteString as B
import qualified Database.PostgreSQL.Simple as SQL
import qualified Data.Time.Clock as Time
import GHC.Generics (Generic)

import Global

data Figure = Figure
  { id :: Integer
  , name :: Text
  , contents :: B.ByteString
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
