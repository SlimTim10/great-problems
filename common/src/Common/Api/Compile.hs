{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
module Common.Api.Compile
  ( OutputOption(..)
  , RandomSeed
  , RequestParam(..)
  ) where

import qualified Data.Aeson as JSON
import GHC.Generics (Generic)

-- deprecated
data OutputOption = WithSolution | WithAnswer | WithSolutionAndAnswer | QuestionOnly
  deriving (Eq, Generic, JSON.FromJSON)
instance Show OutputOption where
  show WithSolution = "flagSolution"
  show WithAnswer = "flagAnswer"
  show WithSolutionAndAnswer = "flagSolAns"
  show QuestionOnly = "flagQuestion"

type RandomSeed = Integer

data RequestParam
  = ParamContents
  | ParamRandomizeVariables
  | ParamOutputOption -- deprecated
  | ParamFigures
  deriving (Eq, Ord)
instance Show RequestParam where
  show ParamContents = "contents"
  show ParamRandomizeVariables = "randomizeVariables"
  show ParamOutputOption = "outputOption" -- deprecated
  show ParamFigures = "figures"
