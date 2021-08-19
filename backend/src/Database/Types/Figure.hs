{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
module Database.Types.Figure
  ( Figure(..)
  ) where

import qualified Database.PostgreSQL.Simple as SQL
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
  )
