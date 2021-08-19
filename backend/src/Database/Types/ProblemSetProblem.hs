{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Database.Types.ProblemSetProblem
  ( ProblemSetProblem(..)
  ) where

import qualified Database.PostgreSQL.Simple as SQL
import GHC.Generics (Generic)

import Global

data ProblemSetProblem = ProblemSetProblem
  { problem_id :: Integer
  , problem_set_id :: Integer
  } deriving
  ( Eq
  , Show
  , Generic
  , SQL.FromRow
  , SQL.ToRow
  )

