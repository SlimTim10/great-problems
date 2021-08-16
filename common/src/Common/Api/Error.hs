{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Common.Api.Error
  ( Error(..)
  , mk
  ) where

import qualified Data.Aeson as JSON
import GHC.Generics (Generic)

import Global

data Error = Error
  { error :: Bool
  , message :: Text
  } deriving
  ( Eq
  , Show
  , Generic
  , JSON.FromJSON
  , JSON.ToJSON
  )

mk :: Text -> Error
mk msg = Error True msg
