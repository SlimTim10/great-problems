{-# LANGUAGE DeriveGeneric #-}
module Common.Compile
  ( CompileRequest(..)
  , CompileResponse(..)
  , OutputOption(..)
  ) where

import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.Aeson as JSON
import qualified GHC.Generics as Generics

import qualified Common.File as File
import Global

data OutputOption = WithSolution | WithAnswer | WithSolutionAndAnswer | QuestionOnly
instance Show OutputOption where
  show WithSolution = "flagSolutions"
  show WithAnswer = "flagAnswers"
  show WithSolutionAndAnswer = "flagSolAns"
  show QuestionOnly = "flagQuestions"

data CompileRequest = CompileRequest
  { prbText :: Text
  , prbName :: Text
  , randomizeVariables :: Bool
  , outputOption :: OutputOption
  , figures :: [File.FileWithName]
  }

data CompileResponse = CompileResponse
  { errorIcemaker :: Text
  , errorLatex :: Text
  , pdfContent :: Text
  , pdfName :: Text
  , terminalOutput :: Text
  } deriving (Generics.Generic, Show)

instance JSON.FromJSON CompileResponse where
  parseJSON = JSON.genericParseJSON opts . jsonLower
    where opts = JSON.defaultOptions { JSON.fieldLabelModifier = map Char.toLower }

jsonLower :: JSON.Value -> JSON.Value
jsonLower (JSON.Object o) = JSON.Object . HM.fromList . map lowerPair . HM.toList $ o
  where lowerPair (key, val) = (T.toLower key, val)
jsonLower x = x

