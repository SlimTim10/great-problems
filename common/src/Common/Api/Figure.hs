{-# LANGUAGE OverloadedStrings #-}
module Common.Api.Figure
  ( FileMap
  , Figure(..)
  ) where

import qualified Data.Aeson as JSON
import qualified Data.Map as Map
import qualified Data.ByteString as B
import qualified Data.Time.Clock as Time
import Data.Aeson ((.:), (.=))

import Global

type FileMap = Map.Map Int Figure

data Figure = Figure
  { id :: Integer
  , name :: Text
  , contents :: B.ByteString
  , createdAt :: Time.UTCTime
  , updatedAt :: Time.UTCTime
  }

instance Show Figure where
  show = show . name

instance Eq Figure where
  (==) a b = name a == name b

instance JSON.FromJSON Figure where
  parseJSON = JSON.withObject "figure" $ \o -> do
    id' <- o .: "id"
    name' <- o .: "name"
    createdAt' <- o .: "createdAt"
    updatedAt' <- o .: "updatedAt"
    return $ Figure id' name' B.empty createdAt' updatedAt'

instance JSON.ToJSON Figure where
  toJSON (Figure id' name' _ createdAt' updatedAt') = JSON.object
    [ "id" .= id'
    , "name" .= name'
    , "createdAt" .= createdAt'
    , "updatedAt" .= updatedAt'
    ]
