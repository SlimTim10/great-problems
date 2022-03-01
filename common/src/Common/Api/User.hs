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
import GHC.Generics (Generic)

import qualified Common.Api.Role as Role
import qualified Common.Api.Email as Email

type FullName = CI Text

data User = User
  { id :: Integer
  , fullName :: FullName
  , email :: Email.Email
  , role :: Role.Role
  , verified :: Bool
  } deriving
  ( Eq
  , Show
  , Generic
  , JSON.FromJSON
  , JSON.ToJSON
  )

-- TODO: move to request
data UpdateRequest = UpdateRequest
  { urRole :: Role.Role
  } deriving
  ( Generic
  , JSON.ToJSON
  , JSON.FromJSON
  )

-- TODO: move to request
data ResetPasswordRequest = ResetPasswordRequest
  { rprEmail :: Email.Email
  } deriving
  ( Generic
  , JSON.ToJSON
  , JSON.FromJSON
  )
