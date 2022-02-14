{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Common.Api.Problem where

import Common.Lib.Prelude

import qualified Data.Map as Map
import qualified Data.Aeson as JSON
import qualified Data.Time.Clock as Time
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
  , topic :: Topic.Topic
  , author :: User.User
  , topicPath :: [Topic.Topic]
  , figures :: [Figure.Figure]
  , status :: ProblemStatus.Status
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
  { gpTopic :: Maybe Integer -- Filter by topic ID
  , gpAuthor :: Maybe Integer -- Filter by author ID
  , gpStatus :: Maybe Integer -- Filter by status ID
  }

getParamsDefault :: GetParams
getParamsDefault = GetParams
  { gpTopic = Nothing
  , gpAuthor = Nothing
  , gpStatus = Just . fromIntegral . fromEnum $ ProblemStatus.Published
  }

getParamsToRouteQuery :: GetParams -> Route.Query
getParamsToRouteQuery gps = Map.fromList
  [ ("topic", gpTopic gps >>= Just . cs . show)
  , ("author", gpAuthor gps >>= Just . cs . show)
  , ("status", gpStatus gps >>= Just . cs . show)
  ]

data BareProblem = BareProblem
  { bpProblemId :: Maybe Integer
  , bpSummary :: Text
  , bpContents :: Text
  , bpTopicId :: Integer
  , bpAuthorId :: Integer
  , bpStatus :: ProblemStatus.Status
  , bpFigures :: [Figure.BareFigure]
  }

data RequestParam
  = ParamProblemId
  | ParamSummary
  | ParamContents
  | ParamTopicId
  | ParamAuthorId
  | ParamStatus
  | ParamFigures
  deriving (Eq, Ord)
instance Show RequestParam where
  show ParamProblemId = "problemId"
  show ParamSummary = "summary"
  show ParamContents = "contents"
  show ParamTopicId = "topicId"
  show ParamAuthorId = "authorId"
  show ParamStatus = "status"
  show ParamFigures = "figures"
