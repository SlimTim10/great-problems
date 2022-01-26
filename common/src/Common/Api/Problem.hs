{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Common.Api.Problem where

import Common.Lib.Prelude

import qualified Data.Map as Map
import qualified Data.Aeson as JSON
import qualified Data.Time.Clock as Time
import qualified Data.Text as Text
import GHC.Generics (Generic)

import qualified Common.Api.Topic as Topic
import qualified Common.Api.ProblemStatus as ProblemStatus
import qualified Common.Api.User as User
import qualified Common.Api.Figure as Figure
import qualified Common.Route as Route

data Problem = Problem
  { id :: Integer
  , summary :: Text
  , contents :: Text
  , topic :: Either Integer Topic.Topic
  , author :: Either Integer User.User
  , topicPath :: Maybe [Topic.Topic]
  , figures :: [Figure.Figure]
  , status :: Either Integer ProblemStatus.Status
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
getParamsToRouteQuery gps = Map.fromList
  [ ("topic", gpTopic gps >>= \t -> Just (cs $ show t))
  , ("expand", gpExpand gps >>= \xs -> Just (Text.intercalate "," xs))
  , ("include", gpInclude gps >>= \case TopicPath -> Just "topic_path")
  ]

data BareProblem = BareProblem
  { bpProblemId :: Maybe Integer
  , bpSummary :: Text
  , bpContents :: Text
  , bpTopicId :: Integer
  , bpAuthorId :: Integer
  , bpStatusId :: Integer
  , bpFigures :: [Figure.BareFigure]
  }

data RequestParam
  = ParamProblemId
  | ParamSummary
  | ParamContents
  | ParamTopicId
  | ParamAuthorId
  | ParamStatusId
  | ParamFigures
  deriving (Eq, Ord)
instance Show RequestParam where
  show ParamProblemId = "problemId"
  show ParamSummary = "summary"
  show ParamContents = "contents"
  show ParamTopicId = "topicId"
  show ParamAuthorId = "authorId"
  show ParamStatusId = "statusId"
  show ParamFigures = "figures"
