{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Common.Api.ProblemSet where

import Common.Lib.Prelude

import qualified Data.Aeson as JSON
import qualified Data.Time.Clock as Time
import qualified Common.Api.User as User
import GHC.Generics (Generic)

data ProblemSet = ProblemSet
  { id :: Integer
  , summary :: Text
  , author :: User.User
  , createdAt :: Time.UTCTime
  , updatedAt :: Time.UTCTime
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
  }

data ProblemSetWithProblems = ProblemSetWithProblems
  { pswpSummary :: Text
  , pswpUpdatedAt :: Time.UTCTime
  , pswpProblems :: [ProblemId]
  }

data ProblemId = Integer

data RequestParam
  = ParamProblemId
  | ParamSummary
  deriving (Eq, Ord)
instance Show RequestParam where
  show ParamProblemId = "problemId"
  show ParamSummary = "summary"
