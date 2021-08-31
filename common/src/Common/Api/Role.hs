{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Common.Api.Role
  ( Role(..)
  ) where

import qualified Data.Aeson as JSON
import GHC.Generics (Generic)
import qualified Servant.Auth.Server as SAS

data Role = User | Contributor | Moderator
  deriving
  ( Eq
  , Show
  , Generic
  , JSON.FromJSON
  , JSON.ToJSON
  , SAS.FromJWT
  , SAS.ToJWT
  )
