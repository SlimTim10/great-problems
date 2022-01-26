{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Common.Api.ProblemStatus
  ( Status(..)
  ) where

import qualified Data.Aeson as JSON
import GHC.Generics (Generic)

data Status = Draft | Published
  deriving
  ( Eq
  , Show
  , Read
  , Ord
  , Enum
  , Bounded
  , Generic
  , JSON.FromJSON
  , JSON.ToJSON
  )
