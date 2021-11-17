{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Common.Api.User
  ( User(..)
  , UpdateRequest(..)
  , ResetPasswordRequest(..)
  ) where

import Common.Lib.Prelude

import qualified Data.Aeson as JSON
import qualified Data.CaseInsensitive as CI
import GHC.Generics (Generic)

import qualified Common.Api.Role as Role

data User = User
  { id :: Integer
  , fullName :: CI Text
  , email :: CI Text
  , role :: Role.Role
  , verified :: Bool
  } deriving
  ( Eq
  , Show
  , Generic
  , JSON.FromJSON
  , JSON.ToJSON
  )

instance (JSON.FromJSON (CI Text)) where
  parseJSON (JSON.String text) = pure $ CI.mk text
  parseJSON v = fail $ "Expected String, encountered " ++ (show v)

instance (JSON.ToJSON (CI Text)) where
  toJSON a = JSON.String (CI.original a)

data UpdateRequest = UpdateRequest
  { urRole :: Role.Role
  } deriving
  ( Generic
  , JSON.ToJSON
  , JSON.FromJSON
  )

data ResetPasswordRequest = ResetPasswordRequest
  { rprEmail :: CI Text
  } deriving
  ( Generic
  , JSON.ToJSON
  , JSON.FromJSON
  )
