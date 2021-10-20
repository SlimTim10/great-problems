module Problem.FormFile
  ( FormFileMap
  , FormFile(..)
  ) where

import qualified Data.Map as Map
import qualified GHCJS.DOM.Types
import Data.Text (Text)

type FormFileMap = Map.Map Int FormFile

data FormFile = FormFile
  { file :: GHCJS.DOM.Types.File
  , name :: Text
  }

instance Show FormFile where
  show = show . name

instance Eq FormFile where
  (==) a b = name a == name b
