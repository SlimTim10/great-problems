module Problem.Types
  ( Options(..)
  , FileWithName(..)
  , ConvertResponse(..)
  ) where

import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM

import qualified GHC.Generics as Generics
import qualified Data.Aeson as JSON
import qualified JSDOM.Types
import qualified Reflex.Dom.Core as R

import Global

data Options t = Options
  { random :: R.Dynamic t Bool
  , output :: R.Dynamic t Text
  , files :: R.Dynamic t [FileWithName]
  }

data FileWithName = FileWithName
  { file :: JSDOM.Types.File
  , name :: Text
  }

instance Show FileWithName where
  show = show . name

instance Eq FileWithName where
  (==) a b = name a == name b

data ConvertResponse = ConvertResponse
  { errorIcemaker :: Text
  , errorLatex :: Text
  , pdfContent :: Text
  , pdfName :: Text
  , terminalOutput :: Text
  } deriving (Generics.Generic, Show)

instance JSON.FromJSON ConvertResponse where
  parseJSON = JSON.genericParseJSON opts . jsonLower
    where opts = JSON.defaultOptions { JSON.fieldLabelModifier = map Char.toLower }

jsonLower :: JSON.Value -> JSON.Value
jsonLower (JSON.Object o) = JSON.Object . HM.fromList . map lowerPair . HM.toList $ o
  where lowerPair (key, val) = (T.toLower key, val)
jsonLower x = x
