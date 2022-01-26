{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Database.Types.ProblemStatus
  ( Status(..)
  ) where

import Common.Lib.Prelude

import qualified Database.PostgreSQL.Simple as SQL
import GHC.Generics (Generic)

data Status = Status
  { id :: Integer
  , name :: Text
  } deriving
  ( Eq
  , Show
  , Generic
  , SQL.FromRow
  , SQL.ToRow
  )
