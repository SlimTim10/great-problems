{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Common.Api.Topic
  ( Topic(..)
  , NewTopic(..)
  , TopicWithChildren(..)
  , TopicWithLevel(..)
  , topicsToHierarchy
  , flattenHierarchy
  ) where

import Prelude hiding (id)
import qualified Data.Aeson as JSON
import GHC.Generics (Generic)

import Common.Lib.Prelude

data Topic = Topic
  { id :: Integer
  , name :: Text
  , parentId :: Maybe Integer
  } deriving
  ( Eq
  , Show
  , Generic
  , JSON.FromJSON
  , JSON.ToJSON
  )

-- | Used for creating a new topic. Topic ID is provided by database.
data NewTopic = NewTopic
  { ntName :: Text
  , ntParentId :: Maybe Integer
  } deriving
  ( Eq
  , Show
  , Generic
  , JSON.FromJSON
  , JSON.ToJSON
  )

data TopicWithChildren = TopicWithChildren
  { twcTopic :: Topic
  , twcChildren :: [TopicWithChildren]
  , twcLevel :: Integer
  } deriving (Show)

data TopicWithLevel = TopicWithLevel
  { twlTopic :: Topic
  , twlLevel :: Integer
  } deriving (Show)

-- | Tree-like structure
topicsToHierarchy :: [Topic] -> [TopicWithChildren]
topicsToHierarchy allTopics = map (f 0) rootTopics
  where
    rootTopics = filter (\x -> isNothing (parentId x)) allTopics
    f :: Integer -> Topic -> TopicWithChildren
    f lvl t = TopicWithChildren t (map (f (lvl + 1)) $ getChildren t) lvl
    getChildren :: Topic -> [Topic]
    getChildren t = filter (\x -> parentId x == Just (id t)) allTopics

-- | Flatten the tree-like structure, preserving level information as an integer (root level is 0)
flattenHierarchy
  :: [TopicWithChildren]
  -> [TopicWithLevel]
flattenHierarchy = concatMap f
  where
    f :: TopicWithChildren -> [TopicWithLevel]
    f x =
      [(\x' -> TopicWithLevel (twcTopic x') (twcLevel x')) x]
      ++
      (concatMap f . twcChildren) x
