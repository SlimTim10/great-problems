{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Common.Api.Topic
  ( Topic(..)
  ) where

import qualified Data.Aeson as JSON
import GHC.Generics (Generic)

import Global

data Topic = Topic
  { id :: Integer
  , name :: Text
  , parentId :: Maybe Integer
  } deriving
  ( Eq
  , Show
  , Generic
  , JSON.FromJSON
  , JSON.ToJSON
  )
