{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Common.Api.ProblemStatus
  ( Status(..)
  , toId
  , fromId
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

toId :: Status -> Integer
toId = fromIntegral . fromEnum

fromId :: Integer -> Status
fromId = toEnum . fromIntegral
