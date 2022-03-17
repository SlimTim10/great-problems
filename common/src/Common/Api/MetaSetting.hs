{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Common.Api.MetaSetting
  ( MetaSetting(..)
  , Setting(..)
  ) where

import Common.Lib.Prelude

import qualified Data.Aeson as JSON
import GHC.Generics (Generic)

data MetaSetting = MetaSetting
  { setting :: Setting
  , value :: Text
  } deriving
  ( Eq
  , Show
  , Generic
  , JSON.FromJSON
  , JSON.ToJSON
  )

data Setting
  = ExampleProblemId
  | Dummy -- Placeholder until we add another setting (to force this to be seen as a sum type)
  deriving
    ( Eq
    , Show
    , Read
    , Enum
    , Bounded
    , Generic
    , JSON.FromJSON
    , JSON.ToJSON
    )
