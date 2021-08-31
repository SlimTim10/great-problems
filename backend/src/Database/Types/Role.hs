{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Database.Types.Role
  ( Role(..)
  ) where

import qualified Database.PostgreSQL.Simple as SQL
import GHC.Generics (Generic)

import Global

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
