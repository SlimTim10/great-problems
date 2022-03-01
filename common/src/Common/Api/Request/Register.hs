{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Common.Api.Request.Register
  ( Register(..)
  ) where

import Common.Lib.Prelude

import qualified Data.Aeson as JSON
import GHC.Generics (Generic)

import qualified Common.Api.Email as Email

-- TODO: move to request
data Register = Register
  { fullName :: CI Text
  , email :: Email.Email
  , password :: Text
  } deriving
  ( Eq
  , Show
  , Generic
  , JSON.FromJSON
  , JSON.ToJSON
  )
