{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Common.Api.ChangePassword
  ( ChangePassword(..)
  ) where

import Common.Lib.Prelude

import qualified Data.Aeson as JSON
import GHC.Generics (Generic)

data ChangePassword = ChangePassword
  { oldPassword :: Text
  , newPassword :: Text
  } deriving
  ( Eq
  , Show
  , Generic
  , JSON.FromJSON
  , JSON.ToJSON
  )
