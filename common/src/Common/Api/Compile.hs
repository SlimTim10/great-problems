{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
module Common.Api.Compile
  ( RequestParam(..)
  , Response(..)
  , OutputOption(..)
  , IcemakerResponse(..)
  ) where

import Common.Lib.Prelude

import qualified Data.Aeson as JSON
import GHC.Generics (Generic)
import Data.Aeson ((.:))

data OutputOption = WithSolution | WithAnswer | WithSolutionAndAnswer | QuestionOnly
  deriving (Eq, Generic, JSON.FromJSON)
instance Show OutputOption where
  show WithSolution = "flagSolutions"
  show WithAnswer = "flagAnswers"
  show WithSolutionAndAnswer = "flagSolAns"
  show QuestionOnly = "flagQuestions"

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
  { iceErrorIcemaker :: Text
  , iceErrorLatex :: Text
  , icePdfContents :: Text
  , icePdfName :: Text
  , iceTerminalOutput :: Text
  } deriving
  ( Eq
  , Show
  )

instance JSON.FromJSON IcemakerResponse where
  parseJSON = JSON.withObject "icemakerResponse" $ \o -> do
    errorIcemaker <- o .: "ErrorIcemaker"
    errorLatex <- o .: "ErrorLatex"
    pdfContents <- o .: "PdfContent"
    pdfName <- o .: "PdfName"
    terminalOutput <- o .: "TerminalOutput"
    return $ IcemakerResponse errorIcemaker errorLatex pdfContents pdfName terminalOutput

