{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Common.Api.ProblemSet where

import Common.Lib.Prelude

import qualified Data.Aeson as JSON
import qualified Data.Time.Clock as Time
import qualified Common.Api.User as User
import qualified Common.Api.Problem as Problem
import GHC.Generics (Generic)

data ProblemSet = ProblemSet
  { id :: Integer
  , summary :: Text
  , author :: User.User
  , createdAt :: Time.UTCTime
  , updatedAt :: Time.UTCTime
  , problems :: [Problem.Problem]
  } deriving
  ( Eq
  , Show
  , Generic
  , JSON.FromJSON
  , JSON.ToJSON
  )

data BareProblemSet = BareProblemSet
  { bpsProblemSetId :: Maybe Integer
  , bpsSummary :: Text
  , bpsAuthorId :: Integer
  , bpsProblemIds :: [Integer]
  }

data RequestParam
  = ParamProblemSetId
  | ParamSummary
  | ParamProblemIds
  deriving (Eq, Ord)
instance Show RequestParam where
  show ParamProblemSetId = "problemSetId"
  show ParamSummary = "summary"
  show ParamProblemIds = "problemIds"
