{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Common.Api.Problem
  ( Problem(..)
  ) where

import qualified Data.Aeson as JSON
import qualified Data.Time.Clock as Time
import GHC.Generics (Generic)

import qualified Common.Api.Topic as Topic
import qualified Common.Api.User as User
import Global

data Problem = Problem
  { id :: Integer
  , summary :: Text
  , contents :: Text
  , topic :: Either Integer Topic.Topic
  , author :: Either Integer User.User
  , topicPath :: Maybe [Topic.Topic]
  , created_at :: Time.UTCTime
  , updated_at :: Time.UTCTime
  } deriving
  ( Eq
  , Show
  , Generic
  , JSON.FromJSON
  , JSON.ToJSON
  )
