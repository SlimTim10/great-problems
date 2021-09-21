{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Common.Api.Register
  ( Register(..)
  ) where

import qualified Data.Aeson as JSON
import qualified Data.CaseInsensitive as CI
import GHC.Generics (Generic)

import Global

data Register = Register
  { full_name :: CI Text
  , email :: CI Text
  , password :: Text
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

