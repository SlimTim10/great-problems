{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
module Common.Api.ProblemTile
  ( ProblemTile(..)
  ) where

import qualified Data.Aeson as JSON
import GHC.Generics (Generic)

import qualified Common.Api.Problem as Problem
import qualified Common.Api.Topic as Topic

data ProblemTile = ProblemTile
  { problem :: Problem.Problem
  , topics :: [Topic.Topic]
  } deriving
  ( Eq
  , Show
  , Generic
  , JSON.FromJSON
  , JSON.ToJSON
  )
