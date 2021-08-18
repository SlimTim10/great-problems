{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
module Database.Types.Figure
  ( Figure(..)
  ) where

import qualified Database.PostgreSQL.Simple as SQL
import qualified Data.Aeson as JSON
import qualified Data.CaseInsensitive as CI
import qualified Data.Time.Clock as Time
import GHC.Generics (Generic)

import Global

data Figure = Figure
  { id :: Integer
  , url :: Text
  , problem_id :: Integer
  } deriving
  ( Eq
  , Show
  , Generic
  , SQL.FromRow
  , SQL.ToRow
  , JSON.FromJSON
  , JSON.ToJSON
  )
