{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
module Common.Api.ProblemSetCard
  ( ProblemSetCard(..)
  ) where

import qualified Data.Aeson as JSON
import GHC.Generics (Generic)

import qualified Common.Api.ProblemSet as ProblemSet
import qualified Common.Api.Topic as Topic
import qualified Common.Api.User as User

-- | A problem set card includes the problem set, the topic branch,
-- and the author so it can all be rendered at once.
data ProblemSetCard = ProblemSetCard
  { problemSet :: ProblemSet.ProblemSet
  , topics :: [Topic.Topic]
  , author :: User.User
  } deriving
  ( Eq
  , Show
  , Generic
  , JSON.FromJSON
  , JSON.ToJSON
  )
