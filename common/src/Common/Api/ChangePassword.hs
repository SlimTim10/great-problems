{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Common.Api.ChangePassword
  ( ChangePassword(..)
  , Identification(..)
  ) where

import Common.Lib.Prelude

import qualified Data.Aeson as JSON
import GHC.Generics (Generic)

data ChangePassword = ChangePassword
  { identification :: Identification
  , newPassword :: Text
  } deriving
  ( Eq
  , Show
  , Generic
  , JSON.FromJSON
  , JSON.ToJSON
  )

data Identification = OldPassword Text | ResetSecret Text
  deriving
    ( Eq
    , Show
    , Generic
    , JSON.FromJSON
    , JSON.ToJSON
    )
