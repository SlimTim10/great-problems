{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Common.Api.Compile
  ( Request(..)
  , RequestParam(..)
  , Response(..)
  , OutputOption(..)
  , IcemakerResponse(..)
  ) where

import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.Aeson as JSON
import GHC.Generics (Generic)

import qualified Common.FormFile as FormFile
import Global

data OutputOption = WithSolution | WithAnswer | WithSolutionAndAnswer | QuestionOnly
  deriving (Eq, Generic, JSON.FromJSON)
instance Show OutputOption where
  show WithSolution = "flagSolutions"
  show WithAnswer = "flagAnswers"
  show WithSolutionAndAnswer = "flagSolAns"
  show QuestionOnly = "flagQuestions"

data Request = Request
  { contents :: Text
  , randomizeVariables :: Bool
  , outputOption :: OutputOption
  , figures :: [FormFile.FormFile]
  }

data RequestParam = ParamContents | ParamRandomizeVariables | ParamOutputOption | ParamFigures
  deriving (Eq, Ord)
instance Show RequestParam where
  show ParamContents = "contents"
  show ParamRandomizeVariables = "randomizeVariables"
  show ParamOutputOption = "outputOption"
  show ParamFigures = "figures"

data Response = Response
  { resErrorIcemaker :: Text
  , resErrorLatex :: Text
  , resPdfContents :: Text
  , resTerminalOutput :: Text
  } deriving
  ( Eq
  , Show
  , Generic
  , JSON.FromJSON
  , JSON.ToJSON
  )

data IcemakerResponse = IcemakerResponse
  { errorIcemaker :: Text
  , errorLatex :: Text
  , pdfContent :: Text
  , pdfName :: Text
  , terminalOutput :: Text
  } deriving
  ( Eq
  , Show
  , Generic
  )

instance JSON.FromJSON IcemakerResponse where
  parseJSON = JSON.genericParseJSON opts . jsonLower
    where opts = JSON.defaultOptions { JSON.fieldLabelModifier = map Char.toLower }

jsonLower :: JSON.Value -> JSON.Value
jsonLower (JSON.Object o) = JSON.Object . HM.fromList . map lowerPair . HM.toList $ o
  where lowerPair (key, val) = (T.toLower key, val)
jsonLower x = x

