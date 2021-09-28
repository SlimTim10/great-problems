{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Common.Api.NewProblem where

import qualified Data.Aeson as JSON
import GHC.Generics (Generic)

import Global

data NewProblem = NewProblem
  { summary :: Text
  , contents :: Text
  , topic_id :: Integer
  , author_id :: Integer
  } deriving
  ( Eq
  , Show
  , Generic
  , JSON.FromJSON
  , JSON.ToJSON
  )
