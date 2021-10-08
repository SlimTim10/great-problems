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
import qualified Common.Api.Figure as Figure
import qualified Common.Route as Route
import qualified Common.FormFile as FormFile
import Global

data Problem = Problem
  { id :: Integer
  , summary :: Text
  , contents :: Text
  , topic :: Either Integer Topic.Topic
  , author :: Either Integer User.User
  , topicPath :: Maybe [Topic.Topic]
  , figures :: [Figure.Figure]
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
  , cpContents :: Text
  , cpTopicId :: Integer
  , cpAuthorId :: Integer
  , cpFigures :: [Figure.BareFigure]
  }

data UpdateProblem = UpdateProblem
  { upProblemId :: Integer
  , upSummary :: Text
  , upContents :: Text
  , upTopicId :: Integer
  , upAuthorId :: Integer
  , upFigures :: [Figure.BareFigure]
  }

-- Update or publish new problem
data RequestSave = RequestSave
  { rsProblemId :: Maybe Integer
  , rsSummary :: Text
  , rsContents :: Text
  , rsTopicId :: Integer
  , rsAuthorId :: Integer
  , rsFigures :: [FormFile.FormFile]
  }

data RequestParam
  = ParamProblemId
  | ParamSummary
  | ParamContents
  | ParamTopicId
  | ParamAuthorId
  | ParamFigures
  deriving (Eq, Ord)
instance Show RequestParam where
  show ParamProblemId = "problemId"
  show ParamSummary = "summary"
  show ParamContents = "contents"
  show ParamTopicId = "topicId"
  show ParamAuthorId = "authorId"
  show ParamFigures = "figures"
