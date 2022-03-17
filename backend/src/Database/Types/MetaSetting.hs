{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Database.Types.MetaSetting
  ( MetaSetting(..)
  ) where

import Common.Lib.Prelude

import qualified Database.PostgreSQL.Simple as SQL
import GHC.Generics (Generic)

data MetaSetting = MetaSetting
  { id :: Integer
  , meta_setting :: Text
  , meta_value :: Maybe Text
  } deriving
  ( Eq
  , Show
  , Generic
  , SQL.FromRow
  , SQL.ToRow
  )
