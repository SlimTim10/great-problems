{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Common.Api.Request.ResendEmail
  ( ResendEmail(..)
  ) where

import qualified Data.Aeson as JSON
import GHC.Generics (Generic)

import qualified Common.Api.Email as Email

data ResendEmail = ResendEmail
  { email :: Email.Email
  } deriving
  ( Eq
  , Show
  , Generic
  , JSON.FromJSON
  , JSON.ToJSON
  )
