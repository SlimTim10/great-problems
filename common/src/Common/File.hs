module Common.File
  ( FileMap
  , FileWithName(..)
  ) where

import qualified Data.Map as Map
import qualified JSDOM.Types

import Global

type FileMap = Map.Map Int FileWithName

data FileWithName = FileWithName
  { file :: JSDOM.Types.File
  , name :: Text
  }

instance Show FileWithName where
  show = show . name

instance Eq FileWithName where
  (==) a b = name a == name b
