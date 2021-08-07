{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
module Common.Api.ProblemCard
  ( ProblemCard(..)
  ) where

import qualified Data.Aeson as JSON
import GHC.Generics (Generic)

import qualified Common.Api.Problem as Problem
import qualified Common.Api.Topic as Topic
import qualified Common.Api.User as User

data ProblemCard = ProblemCard
  { problem :: Problem.Problem
  , topics :: [Topic.Topic]
  , author :: User.User
  } deriving
  ( Eq
  , Show
  , Generic
  , JSON.FromJSON
  , JSON.ToJSON
  )
