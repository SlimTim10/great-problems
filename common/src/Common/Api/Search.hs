{-# LANGUAGE OverloadedStrings #-}
module Common.Api.Search
  ( Params(..)
  , Collection(..)
  , paramsFromQuery
  , paramsToQuery
  ) where

import qualified Data.Map as Map

import Common.Lib.Prelude
import qualified Common.Route as Route

data Collection = Problems | ProblemSets | Courses
  deriving (Eq, Show, Read)

data Params = Params
  { query :: Maybe Text
  , topicId :: Maybe Integer
  , authorId :: Maybe Integer
  , collection :: Maybe Collection
  } deriving
  ( Eq
  , Show
  )

paramsFromQuery :: Route.Query -> Params
paramsFromQuery q = do
  Params
    { query = Route.textParamFromQuery "q" q
    , topicId = Route.readParamFromQuery "topic" q
    , authorId = Route.readParamFromQuery "author" q
    , collection = Route.readParamFromQuery "collection" q
    }

paramsToQuery :: Params -> Route.Query
paramsToQuery ps = Map.fromList
  [ ("q", query ps >>= Just . cs)
  , ("topic", topicId ps >>= Just . cs . show)
  , ("author", authorId ps >>= Just . cs . show)
  , ("collection", collection ps >>= Just . cs . show)
  ]
