module Common.FormFile
  ( FormFileMap
  , FormFile(..)
  ) where

import qualified Data.Map as Map
import qualified JSDOM.Types

import Global

type FormFileMap = Map.Map Int FormFile

data FormFile = FormFile
  { file :: JSDOM.Types.File
  , name :: Text
  }

instance Show FormFile where
  show = show . name

instance Eq FormFile where
  (==) a b = name a == name b
