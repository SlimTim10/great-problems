{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Common.Api.User
  ( User(..)
  ) where

import qualified Database.PostgreSQL.Simple as SQL
import qualified Data.Aeson as JSON
import qualified Data.CaseInsensitive as CI
import GHC.Generics (Generic)

import Global

data User = User
  { id :: Integer
  , full_name :: CI Text
  , email :: CI Text
  } deriving
  ( Eq
  , Show
  , Generic
  , SQL.FromRow
  , SQL.ToRow
  , JSON.FromJSON
  , JSON.ToJSON
  )

instance (JSON.FromJSON (CI Text)) where
  parseJSON (JSON.String text) = pure $ CI.mk text
  parseJSON v = fail $ "Expected String, encountered " ++ (show v)

instance (JSON.ToJSON (CI Text)) where
  toJSON a = JSON.String (CI.original a)

