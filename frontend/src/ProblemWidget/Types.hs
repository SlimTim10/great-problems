{-# LANGUAGE DeriveGeneric #-}

module ProblemWidget.Types where

import Data.Char (toLower)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM

import GHC.Generics (Generic)
import Data.Aeson
  ( FromJSON
  , fieldLabelModifier
  , parseJSON
  , genericParseJSON
  , defaultOptions
  )
import qualified Data.Aeson as JSON
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

data ConvertResponse = ConvertResponse
  { errorIcemaker :: Text
  , errorLatex :: Text
  , pdfContent :: Text
  , pdfName :: Text
  , terminalOutput :: Text
  } deriving (Generic, Show)

instance FromJSON ConvertResponse where
  parseJSON = genericParseJSON opts . jsonLower
    where opts = defaultOptions { fieldLabelModifier = map toLower }

jsonLower :: JSON.Value -> JSON.Value
jsonLower (JSON.Object o) = JSON.Object . HM.fromList . map lowerPair . HM.toList $ o
  where lowerPair (key, val) = (T.toLower key, val)
jsonLower x = x
