{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.Types.ProblemSet
  ( ProblemSet(..)
  ) where

import qualified Database.PostgreSQL.Simple as SQL
import qualified Data.Time.Clock as Time
import GHC.Generics (Generic)

import Global

data ProblemSet = ProblemSet
  { id :: Integer
  , summary :: Text
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
