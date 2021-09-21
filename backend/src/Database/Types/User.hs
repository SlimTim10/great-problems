{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Database.Types.User
  ( User(..)
  ) where

import qualified Database.PostgreSQL.Simple as SQL
import GHC.Generics (Generic)

import Global

data User = User
  { id :: Integer
  , full_name :: CI Text
  , email :: CI Text
  , password :: Text
  , role_id :: Integer
  , verified :: Bool
  } deriving
  ( Eq
  , Show
  , Generic
  , SQL.FromRow
  , SQL.ToRow
  )
