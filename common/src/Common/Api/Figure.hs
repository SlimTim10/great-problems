{-# LANGUAGE OverloadedStrings #-}
module Common.Api.Figure
  ( FileMap
  , Figure(..)
  , BareFigure(..)
  ) where

import Prelude hiding (id)
import Common.Lib.Prelude

import qualified Data.Aeson as JSON
import qualified Data.Map as Map
import qualified Data.ByteString as B
import qualified Data.Time.Clock as Time
import Data.Aeson ((.:), (.=))

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
  (==) a b = (id a == id b)
    && (name a == name b)
    && (createdAt a == createdAt b)
    && (updatedAt a == updatedAt b)

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

data BareFigure = BareFigure
  { bfName :: Text
  , bfContents :: B.ByteString
  }
