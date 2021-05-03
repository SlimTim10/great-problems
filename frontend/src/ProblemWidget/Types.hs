module ProblemWidget.Types
  ( Options(..)
  , FileWithName(..)
  ) where

import Data.Text (Text)

import JSDOM.Types (File)

import Reflex.Dom.Core

data Options t = Options
  { random :: Dynamic t Bool
  , output :: Dynamic t Text
  , files :: Dynamic t [FileWithName]
  }

data FileWithName = FileWithName
  { file :: File
  , name :: Text
  }

instance Show FileWithName where
  show = show . name

instance Eq FileWithName where
  (==) a b = name a == name b
