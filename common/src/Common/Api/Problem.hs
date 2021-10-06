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
import qualified Common.File as File
import Global

data Problem = Problem
  { id :: Integer
  , summary :: Text
  , content :: Text
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

data GetParams = GetParams
  { gpExpand :: Maybe [Text] -- Expand topic or author (instead of only IDs)
  , gpInclude :: Maybe GetParamInclude -- Include extra information
  , gpTopic :: Maybe Integer -- Filter by topic ID
  }

data GetParamInclude = TopicPath

getParamsToRouteQuery :: GetParams -> Route.Query
getParamsToRouteQuery gps =
  ( "topic" =: (gpTopic gps >>= \t -> Just (cs $ show t))
    <> "expand" =: (gpExpand gps >>= \xs -> Just (Text.intercalate "," xs))
    <> "include" =: (gpInclude gps >>= \case TopicPath -> Just "topic_path")
  )

data CreateProblem = CreateProblem
  { cpSummary :: Text
  , cpContent :: Text
  , cpTopicId :: Integer
  , cpAuthorId :: Integer
  }

data UpdateProblem = UpdateProblem
  { upProblemId :: Integer
  , upSummary :: Text
  , upContent :: Text
  , upTopicId :: Integer
  , upAuthorId :: Integer
  }

-- Update or publish new problem
data RequestSave = RequestSave
  { rsProblem :: Either CreateProblem UpdateProblem
  , rsFigures :: [File.FileWithName]
  }

data RequestParam
  = ParamProblemId
  | ParamSummary
  | ParamContent
  | ParamTopicId
  | ParamAuthorId
  | ParamFigures
  deriving (Eq, Ord)
instance Show RequestParam where
  show ParamProblemId = "problemId"
  show ParamSummary = "summary"
  show ParamContent = "content"
  show ParamTopicId = "topicId"
  show ParamAuthorId = "authorId"
  show ParamFigures = "figures"
