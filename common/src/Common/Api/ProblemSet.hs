{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Common.Api.ProblemSet
  ( ProblemSet(..)
  ) where

import Common.Lib.Prelude

import qualified Data.Aeson as JSON
import qualified Data.Time.Clock as Time
import GHC.Generics (Generic)

data ProblemSet = ProblemSet
  { id :: Integer
  , summary :: Text
  , authorId :: Integer
  , createdAt :: Time.UTCTime
  , updatedAt :: Time.UTCTime
  } deriving
  ( Eq
  , Show
  , Generic
  , JSON.FromJSON
  , JSON.ToJSON
  )
