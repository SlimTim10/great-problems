{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
module Common.Api.Role
  ( Role(..)
  ) where

import qualified Data.Aeson as JSON
import GHC.Generics (Generic)

data Role = User | Contributor | Moderator | Administrator
  deriving
  ( Eq
  , Show
  , Ord
  , Enum
  , Bounded
  , Generic
  , JSON.FromJSON
  , JSON.ToJSON
  )
