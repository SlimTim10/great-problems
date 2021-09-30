{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Common.Api.Problem where

import qualified Data.Aeson as JSON
import qualified Data.Time.Clock as Time
import qualified Data.Text as Text
import GHC.Generics (Generic)

import qualified Common.Api.Topic as Topic
import qualified Common.Api.User as User
import qualified Common.Route as Route
import Global

data Problem = Problem
  { id :: Integer
  , summary :: Text
  , contents :: Text
  , topic :: Either Integer Topic.Topic
  , author :: Either Integer User.User
  , topicPath :: Maybe [Topic.Topic]
  , createdAt :: Time.UTCTime
  , updatedAt :: Time.UTCTime
  } deriving
  ( Eq
  , Show
  , Generic
  , JSON.FromJSON
  , JSON.ToJSON
  )

data ProblemParams = ProblemParams
  { paramExpand :: Maybe [Text]
  , paramInclude :: Maybe ProblemParamInclude
  , paramTopic :: Maybe Integer
  }

data ProblemParamInclude = TopicPath

problemParamsToRouteQuery :: ProblemParams -> Route.Query
problemParamsToRouteQuery p =
  ( "topic" =: (paramTopic p >>= \t -> Just (cs $ show t))
    <> "expand" =: (paramExpand p >>= \xs -> Just (Text.intercalate "," xs))
    <> "include" =: (paramInclude p >>= \case TopicPath -> Just "topic_path")
  )
