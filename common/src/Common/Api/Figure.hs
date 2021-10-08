{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
module Common.Api.Figure
  ( FileMap
  , Figure(..)
  ) where

import qualified Control.Monad.IO.Class as IO
import qualified Data.Aeson as JSON
import qualified Data.Map as Map
import qualified Data.ByteString as B
import qualified Data.Time.Clock as Time
import GHC.Generics (Generic)
import Data.Aeson ((.:))

import Global

type FileMap = Map.Map Int Figure

data Figure = Figure
  { contents :: B.ByteString
  , name :: Text
  , createdAt :: Time.UTCTime
  , updatedAt :: Time.UTCTime
  }

instance Show Figure where
  show = show . name

instance Eq Figure where
  (==) a b = name a == name b

instance JSON.FromJSON Figure where
  parseJSON = JSON.withObject "figure" $ \o -> do
    name <- o .: "name"
    createdAt <- o .: "createdAt"
    updatedAt <- o .: "updatedAt"
    return $ Figure B.empty name createdAt updatedAt
