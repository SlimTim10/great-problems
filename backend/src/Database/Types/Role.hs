{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Database.Types.Role
  ( Role(..)
  ) where

import Common.Lib.Prelude

import qualified Database.PostgreSQL.Simple as SQL
import GHC.Generics (Generic)

data Role = Role
  { id :: Integer
  , name :: Text
  } deriving
  ( Eq
  , Show
  , Generic
  , SQL.FromRow
  , SQL.ToRow
  )
