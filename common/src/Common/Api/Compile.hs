{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
module Common.Api.Compile
  ( RequestParam(..)
  , Response(..)
  , OutputOption(..)
  , Problem2texResponse(..)
  ) where

import Common.Lib.Prelude

import qualified Data.Aeson as JSON
import GHC.Generics (Generic)
import Data.Aeson ((.:))

data OutputOption = WithSolution | WithAnswer | WithSolutionAndAnswer | QuestionOnly
  deriving (Eq, Generic, JSON.FromJSON)
instance Show OutputOption where
  show WithSolution = "flagSolution"
  show WithAnswer = "flagAnswer"
  show WithSolutionAndAnswer = "flagSolAns"
  show QuestionOnly = "flagQuestion"

data RequestParam = ParamContents | ParamRandomizeVariables | ParamOutputOption | ParamFigures
  deriving (Eq, Ord)
instance Show RequestParam where
  show ParamContents = "contents"
  show ParamRandomizeVariables = "randomizeVariables"
  show ParamOutputOption = "outputOption"
  show ParamFigures = "figures"

data Response = Response
  { resErrorProblem2tex :: Text
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

data Problem2texResponse = Problem2texResponse
  { p2tErrorProblem2tex :: Text
  , p2tErrorLatex :: Text
  , p2tPdfContents :: Text
  , p2tPdfName :: Text
  , p2tTerminalOutput :: Text
  } deriving
  ( Eq
  , Show
  )

instance JSON.FromJSON Problem2texResponse where
  parseJSON = JSON.withObject "icemakerResponse" $ \o -> do
    errorProblem2tex <- o .: "ErrorIcemaker"
    errorLatex <- o .: "ErrorLatex"
    pdfContents <- o .: "PdfContent"
    pdfName <- o .: "PdfName"
    terminalOutput <- o .: "TerminalOutput"
    return $ Problem2texResponse errorProblem2tex errorLatex pdfContents pdfName terminalOutput

