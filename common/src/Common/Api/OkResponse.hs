{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Common.Api.OkResponse
  ( OkResponse(..)
  ) where

import qualified Data.Aeson as JSON
import GHC.Generics (Generic)

data OkResponse = OkResponse
  deriving
  ( Eq
  , Show
  , Generic
  , JSON.FromJSON
  , JSON.ToJSON
  )
